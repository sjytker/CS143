(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)

class ListNode {
   val : String;
   next : ListNode;

   init(i : String, nxt : ListNode) : ListNode {
      {
         val <- i;
         next <- nxt;
         self;
      }
   };

   next() : ListNode { next };

   val() : String { val };
};

class Stack inherits IO {
   head : ListNode;
   cvt : A2I <- new A2I;

   isNil() : Bool {
      {
         if (isvoid head) then 
            true
         else
            false
         fi;
      }
   };

   top() : ListNode { head };

   push(s : String) : Object {
      head <- (new ListNode).init(s, head)
   };

   pop() : Stack {
      {
         if (not isvoid head) then 
         {
            head <- head.next();
            self;
         }
         else
            self
         fi;
      }
   };

   show() : Object {
      let cur : ListNode <- head in {
         while (not isvoid cur) loop {     
            out_string(cur.val().concat("\n"));
            cur <- cur.next();
         } pool;
         self;
      }
   };

   eval() : Object {
      -- e1 : ListNode;
      -- e2 : ListNode;
      -- e : ListNode;   
      -- Can't declare variables in a function like above, according to P17 of cool-mannual.pdf
      -- But you can declare them using LET token.

      let 
         e1 : ListNode, 
         e2 : ListNode, 
         e : ListNode,
         t : ListNode <- self.top()
      in 
      {
         if (isvoid t) then
            self
         else {
            if (t.val() = "s") then {
               self.pop();
               e1 <- self.top();
               self.pop();
               e2 <-self.top();
               self.pop();
               self.push(e1.val());
               self.push(e2.val());
            }
            else if (t.val() = "+") then {
               self.pop();
               e1 <- self.top();
               self.pop();
               e2 <-self.top();
               self.pop();
               self.push(cvt.i2a(cvt.a2i(e1.val()) + cvt.a2i(e2.val())));
            } else
               self
            fi fi;
         }
         fi;
      }
   };
};



class Main inherits IO {
   st : Stack <- new Stack;
   flag : Bool <- true;
   x : String;
   atoi : A2I <- new A2I;

   main() : Object {
      {
         while (flag) loop {
            out_string(">");
            x <- in_string();
            if (x = "x") then 
               flag <- false
            else if (x = "d") then {
               st.show();
            } else if (x = "e") then {
               st.eval();
            } else {
               st.push(x);
            } fi fi fi;
         } 
         pool;
      }
   };
};
