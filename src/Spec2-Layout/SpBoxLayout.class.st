"
A layout that will arrange presenters in a box, vertically (top to bottom) or horizontally (left to right).
Elements can be added at the begining or at the end of the box (see `SpBoxLayout>>#add:` and `SpBoxLayout>>#addLast:` method families).

## Discussion

The box layout is the main layout in Spec, because its versatility and easy to use. While one may think its functionality is limited, the fact that it can be composed with other types of layouts or other instances of itself (other boxes), will let users do almost everything they need.
The basic usage is, however, very simple: 

```Smalltalk
SpBoxLayout newTopToBottom
	add: aPresenter;
	add: otherPresenter;
	yourself.
```

This will arrange `aPresenter` and `otherPresenter` vertically, assigning to each presenter as much `height` as they can receive (usually 50% of the box).  
Of course, being able to do _just that_ would be very limiting, but then is where layout properties start to be important.

### The expand property (`SpBoxLayout>>#add:expand:`)

With the previous example, let's define a some real presenters: 

```Smalltalk
| presenter |

presenter := SpPresenter new.
presenter layout: (SpBoxLayout newTopToBottom
	add: presenter newTextInput;
	add: presenter newText;
	yourself).
		
presenter openWithSpec
```

What happened when we opened it? Well, the box layout was filled with an input text and a text area, but the height assigned to the first presenter is not what we would have expected, since even if text input is smaller, area height assigned is still half.
The property expand fixes this problem, by telling the layout not to assign more height than what is explicitly needed by the contained presenter: 

```Smalltalk
| presenter |

presenter := SpPresenter new.
presenter layout: (SpBoxLayout newTopToBottom
	add: presenter newTextInput expand: false;
	add: presenter newText;
	yourself).
		
presenter openWithSpec
```

Now the presenter will behave as one would expect (This code is in `SpBoxLayout class>>#exampleExpand`).

### adding presenters **last**.

Now, consider this problem: You want to add something *at the end* of your presenter (for example, you want to add a button, making a dialog): 

```Smalltalk
| presenter |

presenter := SpPresenter new.
presenter layout: (SpBoxLayout newTopToBottom
	add: 'Enter text' expand: false;
	add: presenter newTextInput expand: false;
	add: (presenter newButton label: 'Ok') expand: false;
	yourself).
		
presenter openWithSpec
```

But you want that button to be shown at the end. This is why we use the `addLast:` method, who will instruct the layout to add this presenter at the end.

```Smalltalk
| presenter |

presenter := SpPresenter new.
presenter layout: (SpBoxLayout newTopToBottom
	add: 'Enter text' expand: false;
	add: presenter newTextInput expand: false;
	addLast: (presenter newButton label: 'Ok') expand: false;
	yourself).
		
presenter openWithSpec
```

That makes the presenter work as expected  (This code is in SpBoxLayout class>>#exampleAddLast).

### Composing layouts

The real power of a layout comes when you understand they can be composed (with other instances of themselves or any other kind, children of `SpExecutableLayout`).
For example, taking the previous example, let's say you want to add two buttons (Ok, Cancel), aligned horizontaly. To accomplish that, you add a new layout with the buttons: 

```Smalltalk
| presenter |

presenter := SpPresenter new.
presenter layout: (SpBoxLayout newTopToBottom
	add: 'Enter text' expand: false;
	add: presenter newTextInput expand: false;
	addLast: (SpBoxLayout newLeftToRight
			addLast: (presenter newButton label: 'Ok') expand: false;
			addLast: (presenter newButton label: 'Cancel') expand: false;
			yourself)
		expand: false;
	yourself).
		
presenter openWithSpec
```

Now, the presenter will show buttons at bottom-right corner (This code is in `SpBoxLayout class>>#exampleComposite`).
"
Class {
	#name : #SpBoxLayout,
	#superclass : #SpDirectionableLayout,
	#traits : 'SpTAlignable',
	#classTraits : 'SpTAlignable classTrait',
	#instVars : [
		'spacing',
		'borderWidth',
		'homogeneous'
	],
	#category : #'Spec2-Layout-Box'
}

{ #category : #private }
SpBoxLayout >> adapterName [

	^ #BoxAdapter
]

{ #category : #'api-adding' }
SpBoxLayout >> add: aPresenterLayoutOrSymbol [
	"Adds `aPresenterLayoutOrSymbol` to the list of presenters to be arranged in the layout. 
	 `aPresenterLayoutOrSymboll` can be
		- any instance of `SpPresenter` hierarchy, 
		- another layout or 
		- a Symbol, matching the name of the instance variable who will contain the element to add."

	self 
		add: aPresenterLayoutOrSymbol 
		withConstraints: [ :constraints | ]
]

{ #category : #'api-adding' }
SpBoxLayout >> add: aPresenterLayoutOrSymbol expand: shouldExpand [
	"Adds `aPresenterLayoutOrSymbol` to the list of presenters to be arranged in the layout, setting 
	 also the expand property. 
	 `aPresenterLayoutOrSymboll` can be
		- any instance of `SpPresenter` hierarchy, 
		- another layout or 
		- a Symbol, matching the name of the instance variable who will contain the element to add.
	`shouldExpand` is a boolean indicating whether the new child is to be given extra space allocated 
	to the box."

	^ self 
		add: aPresenterLayoutOrSymbol 
		withConstraints: [ :constraints | 
			constraints expand: shouldExpand ]
]

{ #category : #private }
SpBoxLayout >> add: aName expand: shouldExpand fill: shouldFill [

	self add: aName withConstraints: [ :constraints | 
		constraints
			expand: shouldExpand;
			fill: shouldFill ]
]

{ #category : #private }
SpBoxLayout >> add: aName expand: shouldExpand fill: shouldFill padding: aNumber [
"
aName 		- the presenter to be added to box
expand 	- true if the new child is to be given extra space allocated to box . 
			  The extra space will be divided evenly between all children that use this option
fill 		- true if space given to child by the expand option is actually allocated to child , 
			  rather than just padding it. This parameter has no effect if expand is set to false. 
padding 	- extra space in pixels to put between this child and its neighbors, over and above 
			  the global amount specified by “spacing” property. If child is a widget at one of 
			  the reference ends of box , then padding pixels are also put between child and the
			  reference edge of box"

	self 
		add: aName 
		withConstraints: [ :constraints |
			constraints 
				expand: shouldExpand;
				fill: shouldFill; 
				padding: aNumber ]
]

{ #category : #'api-adding' }
SpBoxLayout >> add: aPresenterLayoutOrSymbol height: anInteger [ 
	"Add a presenter and assign a specific height. 
	 This constraint is applied just when using a Vertical (Top to Bottom) layout, 
	 otherwise is ignored.
	 Please notice that 99% of the time you DO NOT NEED (and you do not want) to use 
	 a fixed constraint like this one. 
	 Instead, consider using `SpBoxLayout>>#add:expand:` method. "

	self 
		add: aPresenterLayoutOrSymbol 
		withConstraints: [ :constraints | constraints height: anInteger ].
]

{ #category : #'api-adding' }
SpBoxLayout >> add: aPresenterLayoutOrSymbol width: anInteger [
	"Add a presenter and assign a specific width. 
	 This constraint is applied just when using an Horizontal (Left to Right) layout, 
	 otherwise is ignored.
	 Please notice that 99% of the time you DO NOT NEED (and you do not want) to use 
	 a fixed constraint like this one. 
	 Instead, consider using `SpBoxLayout>>#add:expand:` method. "

	self
		add: aPresenterLayoutOrSymbol
		withConstraints: [ :constraints | constraints width: anInteger ].
]

{ #category : #'api-adding' }
SpBoxLayout >> add: aPresenterLayoutOrSymbol withConstraints: aBlock [
	"Adds `aPresenterLayoutOrSymbol` to the list of presenters to be arranged in the layout, updating 
	 the constraints directly. 
	 `aPresenterLayoutOrSymboll` can be
		- any instance of `SpPresenter` hierarchy, 
		- another layout or 
		- a Symbol, matching the name of the instance variable who will contain the element to add.
	`aBlock` receives one argument (an instance of `SpBoxConstraints`), to be modified directly.
	
	NOTICE that the direct usage of this method is NOT recommended. Consider using add:/add:expand: 
	instead"
	
	super add: aPresenterLayoutOrSymbol withConstraints: aBlock.
	self announceChildAdded: aPresenterLayoutOrSymbol
]

{ #category : #'api-adding' }
SpBoxLayout >> addLast: aName [
	"Adds `aPresenterLayoutOrSymbol` to the **end of the list** of presenters to be arranged 
	 in the layout. 
	 `aPresenterLayoutOrSymboll` can be
		- any instance of `SpPresenter` hierarchy, 
		- another layout or 
		- a Symbol, matching the name of the instance variable who will contain the element to add."

	self 
		addLast: aName 
		withConstraints: [ :constraints | ]
]

{ #category : #'api-adding' }
SpBoxLayout >> addLast: aName expand: shouldExpand [
	"Adds `aPresenterLayoutOrSymbol` to the **end of the list** of presenters to be arranged 
	 in the layout, setting also the expand property. 
	 `aPresenterLayoutOrSymboll` can be
		- any instance of `SpPresenter` hierarchy, 
		- another layout or 
		- a Symbol, matching the name of the instance variable who will contain the element to add.
	`shouldExpand` is a boolean indicating whether the new child is to be given extra space allocated 
	to the box."

	self 
		addLast: aName 
		withConstraints: [ :constraints | 
			constraints expand: shouldExpand ]
]

{ #category : #private }
SpBoxLayout >> addLast: aName expand: shouldExpand fill: shouldFill [

	self 
		addLast: aName 
	   withConstraints: [ :constraints | 
			constraints
				expand: shouldExpand;
				fill: shouldFill ]
]

{ #category : #private }
SpBoxLayout >> addLast: aName expand: shouldExpand fill: shouldFill padding: aNumber [

	self 
		addLast: aName 
		withConstraints: [ :constraints |
			constraints 
				expand: shouldExpand;
				fill: shouldFill; 
				padding: aNumber ]
]

{ #category : #'api-adding' }
SpBoxLayout >> addLast: aName withConstraints: aBlock [
	"Adds `aPresenterLayoutOrSymbol` to the **end of the list** of presenters to be arranged 
	in the layout, updating the constraints directly. 
	 `aPresenterLayoutOrSymboll` can be
		- any instance of `SpPresenter` hierarchy, 
		- another layout or 
		- a Symbol, matching the name of the instance variable who will contain the element to add.
	`aBlock` receives one argument (an instance of `SpBoxConstraints`), to be modified directly.
	
	NOTICE that the direct usage of this method is NOT recommended. Consider using add:/add:expand: 
	instead"

	self 
		add: aName 
		withConstraints: [ :constraints |
			constraints bePlacedAtEnd.
			aBlock value: constraints ]
]

{ #category : #api }
SpBoxLayout >> beHomogeneous [
	"When a layout us set as homogeneous, all its components have assigned same space, regardless 
	 their own expand or size settings."

	self homogeneous: true
]

{ #category : #api }
SpBoxLayout >> beNotHomogeneous [
	"When a layout us set as not homogeneous, components are ordered respecting their own 
	 properties about expand and/or size assignment. 
	 This is the default."

	self homogeneous: false
]

{ #category : #api }
SpBoxLayout >> borderWidth [
	"Answer the border width defined for the layout (default is zero)"

	^ borderWidth
]

{ #category : #api }
SpBoxLayout >> borderWidth: aNumber [
	"Set the border width to be used by the layout"

	borderWidth := aNumber.
	self withAdapterDo: [ :anAdapter | anAdapter updateBorderWidth ]
]

{ #category : #private }
SpBoxLayout >> constraintsClass [

	^ SpBoxConstraints
]

{ #category : #accessing }
SpBoxLayout >> direction [ 

	^ direction
]

{ #category : #private }
SpBoxLayout >> homogeneous: aBoolean [

	homogeneous := aBoolean
]

{ #category : #initialization }
SpBoxLayout >> initialize [

	super initialize.
	self beNotHomogeneous.
	self spacing: 0.
	self borderWidth: 0
]

{ #category : #testing }
SpBoxLayout >> isHomogeneous [
	"Answer whether the layout should distribute children homogeneous or not"

	^ homogeneous
]

{ #category : #testing }
SpBoxLayout >> isHorizontal [ 
	"Answer true if the layout direction is horizontal"

	^ self direction = SpLayoutDirection horizontal
]

{ #category : #testing }
SpBoxLayout >> isVertical [ 
	"Answer true if the layout direction is vertical"

	^ self direction = SpLayoutDirection vertical
]

{ #category : #'api-adding' }
SpBoxLayout >> removeAll [

	children removeAll.
	adapter ifNotNil: [ :anAdapter | anAdapter removeAll ]
]

{ #category : #'api-adding' }
SpBoxLayout >> replace: aPresenter with: otherPresenter [
	"Replace aPresenter with otherPresenter. 
	 Original constraints will be preserved."
	
	self 
		replace: aPresenter 
		with: otherPresenter
		withConstraints: (self constraintsFor: aPresenter)
]

{ #category : #'api-adding' }
SpBoxLayout >> replace: aPresenter with: otherPresenter withConstraints: constraints [
	"Replace aPresenter with otherPresenter"
	
	children
		replaceKey: aPresenter
		with: otherPresenter.

	self withAdapterDo: [ :anAdapter |
		anAdapter 
			replace: aPresenter 
			with: otherPresenter
			withConstraints: constraints ]
]

{ #category : #api }
SpBoxLayout >> spacing [
	"Answer the spacing between elements of the layout (default is zero)"

	^ spacing
]

{ #category : #api }
SpBoxLayout >> spacing: aNumber [
	"Set the spacing between elements of the layout"

	spacing := aNumber.
	self withAdapterDo: [ :anAdapter | anAdapter updateSpacing ]	
]
