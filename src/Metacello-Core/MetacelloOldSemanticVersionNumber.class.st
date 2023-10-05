Class {
	#name : 'MetacelloOldSemanticVersionNumber',
	#superclass : 'MetacelloSemanticVersionNumber',
	#category : 'Metacello-Core-Model',
	#package : 'Metacello-Core',
	#tag : 'Model'
}

{ #category : 'private' }
MetacelloOldSemanticVersionNumber class >> integerFromString: aString [
  ^ aString asInteger
]

{ #category : 'private' }
MetacelloOldSemanticVersionNumber class >> validateVersionNumber: svn against: aString [
  "no validation"
]
