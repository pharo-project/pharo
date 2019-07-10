"
Example for a simple grid node
"
Class {
	#name : #SimpleGridNodeExample,
	#superclass : #MorphTreeNodeModel,
	#category : #'Morphic-Examples-MorphTreeWidget'
}

{ #category : #accessing }
SimpleGridNodeExample >> age [
	^ self item age
]

{ #category : #accessing }
SimpleGridNodeExample >> age: anInteger [
	self item age: anInteger.
	self changed: #age
]

{ #category : #'accessing - morphs' }
SimpleGridNodeExample >> ageMorph [
	^ ( self theme  
				newTextEntryIn: self currentWorld
				for: self 
				get: #age
				set: #age:
				class: Integer
				getEnabled: nil
				help: nil) color: Color transparent
]

{ #category : #accessing }
SimpleGridNodeExample >> firstName [
	^ self item firstName
]

{ #category : #accessing }
SimpleGridNodeExample >> firstName: aString [
	self item firstName: aString.
	self changed: #firstName
]

{ #category : #'accessing - morphs' }
SimpleGridNodeExample >> firstNameMorph [
	^ ( self theme  
				newTextEntryIn: self currentWorld
				for: self 
				get: #firstName
				set: #firstName:
				class: String
				getEnabled: nil
				help: nil) color: Color transparent
]

{ #category : #accessing }
SimpleGridNodeExample >> married [
	^ self item married
]

{ #category : #accessing }
SimpleGridNodeExample >> married: aBoolean [
	self item married: aBoolean.
	self changed: #married
]

{ #category : #'accessing - morphs' }
SimpleGridNodeExample >> marriedMorph [
	^ ( self theme  
			newCheckboxIn: self currentWorld
			for: self 
			getSelected: #married
			setSelected: #married:
			getEnabled: nil
			label: '' 
			help: nil) color: Color transparent
]

{ #category : #accessing }
SimpleGridNodeExample >> secondName [
	^ self item secondName
]

{ #category : #accessing }
SimpleGridNodeExample >> secondName: aString [
	self item secondName: aString.
	self changed: #secondName
]

{ #category : #'accessing - morphs' }
SimpleGridNodeExample >> secondNameMorph [
	^ ( self theme  
				newTextEntryIn: self currentWorld
				for: self 
				get: #secondName
				set: #secondName:
				class: String
				getEnabled: nil
				help: nil) color: Color transparent
]
