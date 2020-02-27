"
Parent class of all Pharo tutorials.

To create your own tutorial:
- subclass AbstractTutorial
- implement a few methods which returns a Lesson instance
- implement the method #tutorial which returns a collection of selectors that when executed create related lessons.

For example, see MockTutorial (minimalist) and PharoySntaxTutorial (default PharoTutorial one).

See the PharoTutorial class comment to execute your own tutorial.
"
Class {
	#name : #AbstractTutorial,
	#superclass : #Object,
	#category : #'ProfStef-Core'
}

{ #category : #'tutorial metainfo' }
AbstractTutorial class >> contentsOfTutorials [
	^ String
		streamContents: [ :aStream | 
			self tutorials
				do: [ :aTutorial | 
					aStream
						nextPutAll: aTutorial title;
						cr;
						tab;
						nextPutAll: 'ProfStef goOn: ';
						nextPutAll: aTutorial name;
						cr;
						cr ] ]
]

{ #category : #'tutorial metainfo' }
AbstractTutorial class >> title [	
	"Return the title of the tutorial by parsing the class name 	
	like a Wiki link and interspersing whitespaces between the tokens"	
	| className separators groups |	
			
	className := self name.	
	separators := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.	
	groups := className findTokens: separators keep: separators.	
	^' ' join: (groups pairsCollect: [ :sep :rest | sep , rest ]).

]

{ #category : #'tutorial metainfo' }
AbstractTutorial class >> tutorials [
	^ (self subclasses sort: [ :a :b | a name < b name ])
		reject: [ :aTutorial | aTutorial package name = 'ProfStef-Tests' ]
]

{ #category : #tutorial }
AbstractTutorial >> indexOfLesson: aSelector [
	^self tutorial indexOf: aSelector.
]

{ #category : #tutorial }
AbstractTutorial >> lessonAt: anInteger [
	| lessonSelector |
	lessonSelector := self tutorial at: anInteger.
	^ self perform: lessonSelector.
]

{ #category : #accessing }
AbstractTutorial >> lessons [
	^ self tutorial collect: [:aSelector| self perform: aSelector]
]

{ #category : #printing }
AbstractTutorial >> printOn: aStream [
	aStream 
		nextPutAll: 'a Pharo Tutorial (';
		nextPutAll: self class title;
		nextPutAll: ')'.    
]

{ #category : #accessing }
AbstractTutorial >> size [
	^ self tutorial size
]

{ #category : #tutorial }
AbstractTutorial >> tutorial [
	"Should return an Array of selectors which returns Lesson instances.
	See SmalltalkSyntaxTutorial."
	^ self shouldBeImplemented.
]
