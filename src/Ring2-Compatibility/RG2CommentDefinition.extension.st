Extension { #name : #RG2CommentDefinition }

{ #category : #'*Ring2-Compatibility' }
RG2CommentDefinition >> <= aRGCommentDefinition [
	"Sort comment definition according to: 1) name of the class"
	
	^(self parent name <= aRGCommentDefinition parent name)
]

{ #category : #'*Ring2-Compatibility' }
RG2CommentDefinition >> asHistorical [

	| aComment |
	
	aComment := self copy makeResolved.
	aComment environment backend: RG2EnvironmentBackend new. 
	^ aComment.
]

{ #category : #'*Ring2-Compatibility' }
RG2CommentDefinition >> category [
	^self realClass category
]

{ #category : #'*Ring2-Compatibility' }
RG2CommentDefinition >> isFromTrait [

	^ false
]

{ #category : #'*Ring2-Compatibility' }
RG2CommentDefinition >> methodClass [
	^ self realClass
]

{ #category : #'*Ring2-Compatibility' }
RG2CommentDefinition >> realClass [
	^ self parent realClass
]

{ #category : #'*Ring2-Compatibility' }
RG2CommentDefinition class >> realClass: aClass [
	"Creates a ring comment definition from a Smalltalk class"
	
	| env aDefinition |

	env := RG2Environment new.
	aDefinition := aClass asRing2MinimalCommentDefinitionIn: env.
	aDefinition name: 'Comment'.
	env backend: (RG2ReadOnlyImageBackend for: env).
	^ aDefinition
]

{ #category : #'*Ring2-Compatibility' }
RG2CommentDefinition >> selector [

	"f comments are mixed with methods use #name instead"
	^ self name
]
