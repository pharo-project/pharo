Eliminate unnecessary not''s in relation of conditionals.
For example test not ifTrue:[] is equivalent to test ifFalse:[]' 

[[[
anObject not ifFalse: block'
]]]

is transformed into

[[[  
anObject ifTrue: block'
]]]

[[[
aCollection select: [ :each | ... anObject not ] 
]]]
	
is transformed into 	

[[[
aCollection reject: [ :each | ... anObject ] 
]]]