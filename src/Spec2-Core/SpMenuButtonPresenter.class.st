"
I'm a presenter to create a menu button (a button who exposes a menu instead having an action) 

self example
"
Class {
	#name : #SpMenuButtonPresenter,
	#superclass : #SpAbstractButtonPresenter,
	#instVars : [
		'#menu => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpMenuButtonPresenter class >> adapterName [

	^ #MenuButtonAdapter
]

{ #category : #example }
SpMenuButtonPresenter class >> example [
	| menuButton loremIpsumWords |

	loremIpsumWords := String loremIpsum substrings.
	menuButton := SpMenuButtonPresenter new 
		menu: [ SpMenuPresenter new
			addGroup: [ :group | group
				addItem: [ :item | item name: '1: ', loremIpsumWords atRandom ];
				addItem: [ :item | item name: '2: ', loremIpsumWords atRandom ];
				addItem: [ :item | item name: '3: ', loremIpsumWords atRandom ] ];
			yourself ];
		yourself.
		
	menuButton openWithSpec
]

{ #category : #specs }
SpMenuButtonPresenter class >> title [

	^ 'MenuButton'
]

{ #category : #initialization }
SpMenuButtonPresenter >> initialize [
	super initialize.
]

{ #category : #api }
SpMenuButtonPresenter >> menu [

	^ menu
]

{ #category : #api }
SpMenuButtonPresenter >> menu: aValuable [

	menu := aValuable
]

{ #category : #'api-events' }
SpMenuButtonPresenter >> whenMenuChangedDo: aBlock [

	self property: #menu whenChangedDo: aBlock
]
