tests - compare
testSetOfFloat
      "Classical disagreement between hash and = did lead to a bug.
       This is a non regression test from http://bugs.squeak.org/view.php?id=3360"
	"self debug: #testSetOfFloat"

       | size3 size4 |
       size3 := (Set new: 3) add: 3; add: 3.0; size.
       size4 := (Set new: 4) add: 3; add: 3.0; size.
       self assert: size3 = size4 description: 'The size of a Set should not depend on its capacity.'