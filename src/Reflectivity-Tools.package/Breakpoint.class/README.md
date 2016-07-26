A Breakpoint is an object used for stopping the execution of the program in a determined instruction, normally with debugging purposes. 
There are three types of breakpoints:
-halt once: These are triggered once, and then removed.
-halt always: Once installed, they have to be explicitly removed (by using #remove on the instance side, or #removeAll on class side)
-halt on condition: Before being installed prompts the user for a condition (a block with a boolean expression), and it is only triggered when that condition is met.

Breakpoints are installed via SmartSuggestions menu (available for RBMethodNode, RBMessageNode and RBAssignmentNode), or programmatically:

"(Breakpoint inMethod: (Bar>>#foo2) inNode: ((Bar>>#foo2) ast allChildren at: 9) )
break: #when: withArguments: { [ RFCounter counter isZero ] }
"

