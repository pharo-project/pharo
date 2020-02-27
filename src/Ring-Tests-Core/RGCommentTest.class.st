Class {
	#name : #RGCommentTest,
	#superclass : #RGTest,
	#category : #'Ring-Tests-Core'
}

{ #category : #tests }
RGCommentTest >> testClass [

	| environment aClass aPackage aComment |
	
	environment := RGEnvironment new.
	aClass := environment ensureClassNamed: #SomeClass.
	aPackage := environment ensurePackageNamed: #SomePackage.
	aClass package: aPackage.
	aComment := RGComment parent: aClass.
	aComment content: 'some comment'.
	self deny: aClass hasComment.
	aClass comment: aComment.
	self assert: aComment package equals: aPackage.
	self assert: aClass hasComment.
	
	self assert: aComment fullName equals: 'SomeClass comment'.

]

{ #category : #tests }
RGCommentTest >> testImplicitEnvironment [
	
	| def def2 env |

	def := RGComment unnamed.
	env := def environment.

	self checkImplicitSingleClassEnvironmentOf: def parent.	

	self assert: (def isRingResolved).
	
	def2 := RGComment parent: def parent .
	self checkImplicitSingleClassEnvironmentOf: def parent.	

]

{ #category : #tests }
RGCommentTest >> testMakeReslolved [

	| comment |

	comment := RGComment unnamed.
	comment makeResolved.
	self assert: comment unresolvedProperties isEmpty.
	self assert: comment content equals: ''.
	self assert: comment author equals: ''.
	self assert: (comment time isKindOf: DateAndTime).
	
]

{ #category : #tests }
RGCommentTest >> testNewComment [
	| comment time |
	
	comment := RGComment unnamed.
	
	self assert: (comment isRingResolved).
	self assert: (comment isComment).
	
	self assert: (comment hasUnresolvedAll: #(content author time name)).
	
	self assert: (comment isEmptyOrNil).
	
	self assert: comment content equals: ''.
	comment content: 'some class comment'.
	self assert: (comment hasUnresolvedAll: #(author time name)).
	self assert: comment content equals: 'some class comment'.
	self deny: (comment isEmptyOrNil).
	self assert: comment content equals: comment sourceCode.
	
	self assert: comment author equals: ''.
	comment author: 'FullAuthorName'.
	self assert: (comment hasUnresolvedAll: #(time name)).
	self assert: comment author equals: 'FullAuthorName'.
	
	time := DateAndTime now.
	self assert: comment time equals: DateAndTime new.
	comment time: time.
	self assert: (comment hasUnresolved: #name).
	self assert: comment time equals: time.

]

{ #category : #tests }
RGCommentTest >> testResolvingConsistency [

	| anObject |
	anObject := RGComment unnamed.
	self checkClassesConsistency: anObject and: anObject makeResolved
]

{ #category : #tests }
RGCommentTest >> testTransformations [
	| comment time |
	
	comment := RGComment unnamed.
	
	self assert: (comment isRingResolved).
	
	self assert: (comment hasUnresolvedAll: #(content author time name)).
	
	self assert: comment content equals: ''.
	comment content: 'some class comment'.
	self assert: (comment hasUnresolvedAll: #(author time name)).
	self assert: comment content equals: 'some class comment'.
	
	self assert: comment author equals: ''.
	comment author: 'FullAuthorName'.
	self assert: (comment hasUnresolvedAll: #(time name)).
	self assert: comment author equals: 'FullAuthorName'.
	
	time := DateAndTime now.
	self assert: comment time equals: DateAndTime new.
	comment time: time.
	self assert: (comment hasUnresolved: #name).
	self assert: comment time equals: time.

]
