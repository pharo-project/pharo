"
SUnit tests for extensions made by PharoDocComment package
"
Class {
	#name : #PharoDocCommentExtensionsTest,
	#superclass : #TestCase,
	#category : #'PharoDocComment-Tests-Base'
}

{ #category : #tests }
PharoDocCommentExtensionsTest >> testAssociation [

	|association|
	association := 3 + 4 >>> 7.
	self 
		assert: association key equals: 7;
		assert: association value equals: 7
	
]
