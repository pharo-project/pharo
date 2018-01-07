Class {
	#name : #RG2CommentDefinitionTest,
	#superclass : #RG2Test,
	#category : #'Ring2-Tests-Core'
}

{ #category : #tests }
RG2CommentDefinitionTest >> testClass [

	| environment aClass aPackage aComment |
	
	environment := RG2Environment new.
	aClass := environment ensureClassNamed: #SomeClass.
	aPackage := environment ensurePackageNamed: #SomePackage.
	aClass package: aPackage.
	aComment := RG2CommentDefinition parent: aClass.
	aComment content: 'some comment'.
	self deny: aClass hasComment.
	aClass comment: aComment.
	self assert: aComment package equals: aPackage.
	self assert: aClass hasComment.
	
	self assert: aComment fullName equals: 'SomeClass comment'.

]

{ #category : #tests }
RG2CommentDefinitionTest >> testImplicitEnvironment [
	
	| def def2 env |

	def := RG2CommentDefinition unnamed.
	env := def environment.

	self checkImplicitSingleClassEnvironmentOf: def parent.	

	self assert: (def isRing2Resolved).
	
	def2 := RG2CommentDefinition parent: def parent .
	self checkImplicitSingleClassEnvironmentOf: def parent.	

]

{ #category : #tests }
RG2CommentDefinitionTest >> testMakeReslolved [

	| comment |

	comment := RG2CommentDefinition unnamed.
	comment makeResolved.
	self assert: comment unresolvedProperties isEmpty.
	self assert: comment content equals: ''.
	self assert: comment author equals: ''.
	self assert: (comment time isKindOf: DateAndTime).
	
]

{ #category : #tests }
RG2CommentDefinitionTest >> testNewComment [
	| comment time |
	
	comment := RG2CommentDefinition unnamed.
	
	self assert: (comment isRing2Resolved).
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
RG2CommentDefinitionTest >> testTransformations [
	| comment time |
	
	comment := RG2CommentDefinition unnamed.
	
	self assert: (comment isRing2Resolved).
	
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
