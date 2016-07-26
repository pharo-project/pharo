as yet unclassified
testText
	|  t  |

t := 'The safety Dconstraint is that the garbage collector 
MUST NOT collect any storage that then becomes needed to continue correct execution of the program. Should weak references 
be added, then garbage collection decisions become observable. 
Based on a suggestion from Cameron McCormack, we state the 
safety constraint as follows: So long as operational semantics of 
the remainder of the program execution includes the possibility that 
a reference to an object X may be dereferenced, then X MUST NOT 
be collected. Thus, the garbage collector is allowed to ignore any 
references that remain present in the semantic state, but which it 
can ascertain will never be dereferenced in any possible execution. 
This sets an upper bound on what state MAY be collected. Put 
another way, if the garbage collector ever reports that X has been 
collected, such as by nullifying a weak reference to X, if operational 
semantics of remaining execution requires the traversal of a strong 
(non-weak) reference to X, then the previous report demonstrates a 
safety violation. 
' 
asText .
	t addAttribute: (TextFontReference toFont: (
LogicalFont
 familyName: 'Arial'
 pointSize: 8
)).
	t addAttribute: (TextEmphasis italic) from: 5 to: 100.
	t addAttribute: (TextColor new color: (Color red)) from: 10 to: 60.
	t addAttribute: (TextEmphasis italic) from: 100 to: 200.
	t addAttribute: (TextColor new color: Color green) from: 230 to: 360.

	t addAttribute: (
		TextFontReference toFont: ( LogicalFont familyName: 'Tahoma'  pointSize: 14)) from: 88 to: 200.

	^ t