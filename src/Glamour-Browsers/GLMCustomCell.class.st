Class {
	#name : #GLMCustomCell,
	#superclass : #Object,
	#instVars : [
		'children',
		'span',
		'id',
		'browser',
		'size'
	],
	#category : #'Glamour-Browsers-Tabulator'
}

{ #category : #accessing }
GLMCustomCell >> browser [ 
	 
	^browser
]

{ #category : #accessing }
GLMCustomCell >> browser: anObject [ 
	 
	browser := anObject
]

{ #category : #accessing }
GLMCustomCell >> children [ 
	 
	^children ifNil: [children := OrderedCollection new]
]

{ #category : #accessing }
GLMCustomCell >> children: anObject [ 
	 
	children := anObject
]

{ #category : #accessing }
GLMCustomCell >> childrenCount [ 
	 
	^self children 
		inject: 0 
		into: [:sum :each | sum + each span]
]

{ #category : #testing }
GLMCustomCell >> hasId [ 
	 
	^id notNil
]

{ #category : #accessing }
GLMCustomCell >> id [ 
	 
	^id ifNil: [#anonymous]
]

{ #category : #accessing }
GLMCustomCell >> id: anObject [ 
	 
	id := anObject
]

{ #category : #testing }
GLMCustomCell >> isColumn [ 
	 
	^false
]

{ #category : #testing }
GLMCustomCell >> isRow [ 
	 
	^false
]

{ #category : #testing }
GLMCustomCell >> isSplitIntoColumns [ 
	 
	^self children allSatisfy: [:each | each isColumn]
]

{ #category : #testing }
GLMCustomCell >> isSplitIntoRows [ 
	 
	^self children allSatisfy: [:each | each isRow]
]

{ #category : #accessing }
GLMCustomCell >> size [
	^ size ifNil: [0]
]

{ #category : #accessing }
GLMCustomCell >> size: anObject [
	size := anObject.
]

{ #category : #accessing }
GLMCustomCell >> span [ 
	 
	^span ifNil: [span := 1]
]

{ #category : #accessing }
GLMCustomCell >> span: anObject [ 
	 
	span := anObject
]
