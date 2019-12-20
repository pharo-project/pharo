Class {
	#name : #SpTextAdapterTest,
	#superclass : #SpAbstractTextAdapterTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #accessing }
SpTextAdapterTest >> classToTest [

	^ SpTextPresenter
]

{ #category : #accessing }
SpTextAdapterTest >> testKeyBindings [
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
SpTextAdapterTest >> testRightClickShowsMenu [
	| menu |

	presenter contextMenu: (menu := SpMockMenuPresenter new).

	self deny: menu shown.
	self emulateRightClick.
	self assert: menu shown
]
