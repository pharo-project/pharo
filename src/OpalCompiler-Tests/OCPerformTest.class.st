"
I test a primitive failure with perform:withArguments: which was raised when selectors with many arguments was called from methods with small stack sizes.
"
Class {
	#name : #OCPerformTest,
	#superclass : #TestCase,
	#category : 'OpalCompiler-Tests-FromOld'
}

{ #category : #'helper methods' }
OCPerformTest >> a1: a1 a2: a2 a3: a3 a4: a4 a5: a5 a6: a6 a7: a7 a8:a8 a9: a9 a10: a10 
a11: a11 a12: a12 a13: a13 a14: a14 a15: a15 [ 

	^ true
]

{ #category : #'helper methods' }
OCPerformTest >> doPerformOldLargeFrame [
	| t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 |
	"The perform primitive reuses the current stack frame.
	When this was big,the following perform would succeed, since the stack frame was large enough to fit all the arguments"
	
	t1 := t2 := t3 := t4 := t5 := t6 := t7 := t8 := t9 := t10 := t11 := t12 := t13 := 1.
	
	self perform: #a1:a2:a3:a4:a5:a6:a7:a8:a9:a10:a11:a12:a13:a14:a15:
		withArguments: (Array new: 15).

	^ t1
]

{ #category : #'helper methods' }
OCPerformTest >> doPerformOldSmallFrame [
	"The perform primitive reuses the current stack frame.
	When this was small, it would cause the perform primitive to fail, when the amount of arguments were too high"
	
	^self perform: #a1:a2:a3:a4:a5:a6:a7:a8:a9:a10:a11:a12:a13:a14:a15:
		withArguments: (Array new: 15).


]

{ #category : #testing }
OCPerformTest >> testPerformWithArgsLargeFrame [
	self doPerformOldLargeFrame
]

{ #category : #testing }
OCPerformTest >> testPerformWithArgsSmallFrame [
	self doPerformOldSmallFrame
]
