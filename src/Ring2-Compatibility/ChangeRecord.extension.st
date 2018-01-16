Extension { #name : #ChangeRecord }

{ #category : #'*Ring2-Compatibility' }
ChangeRecord >> asRing2Definition [
	"Retrieves a RGMethodDefinition object based on the data of the receiver"
	
	self type == #method
		ifTrue: [ ^self createMethodDefinition ].
	self type == #classComment
		ifTrue: [ ^self createCommentDefinition ].
	^nil
]

{ #category : #'*Ring2-Compatibility' }
ChangeRecord >> createCommentDefinition [
	"Retrieves a RGCommentDefinition object based on the data of the receiver"

	^ (RG2CommentDefinition named: 'Comment')
		parent name: self commentClass;
		content: self string;
		stamp: stamp;
		yourself 
]

{ #category : #'*Ring2-Compatibility' }
ChangeRecord >> createMethodDefinition [
	"Retrieves a RGMethodDefinition object based on the data of the receiver"
	
	| aDefinition |
	
	aDefinition := RG2MethodDefinition named: self methodSelector.
	aDefinition parent name: self methodClassName.
	aDefinition
"		isMetaSide: meta;  "
		sourceCode: self string;
		protocol: category;
		"stamp: stamp;"
		yourself.
	^ aDefinition
]
