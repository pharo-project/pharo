Class {
	#name : #TextAdapterTest,
	#superclass : #AbstractWidgetAdapterTestCase,
	#category : #'Spec-BackendTests'
}

{ #category : #accessing }
TextAdapterTest >> classToTest [

	^ TextPresenter
]

{ #category : #accessing }
TextAdapterTest >> testKeyBindings [
	| handled |

	handled := false.
	presenter 
		bindKeyCombination: $t meta 
		toAction: [ handled := true ].

	self adapter 
		keyPressed: $t asciiValue 
		shift: false 
		meta: true
		control: false
		option: false. 

	self assert: handled
]

{ #category : #accessing }
TextAdapterTest >> testRightClickShowsMenu [
	| menu |

	presenter contextMenu: (menu := SpecMockMenuPresenter new).

	self deny: menu shown.
	self emulateRightClick.
	self assert: menu shown
]
