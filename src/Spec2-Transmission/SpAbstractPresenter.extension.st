Extension { #name : #SpAbstractPresenter }

{ #category : #'*Spec2-Transmission' }
SpAbstractPresenter >> defineInputPorts [

	^ #()
]

{ #category : #'*Spec2-Transmission' }
SpAbstractPresenter >> defineOutputPorts [

	^ #()
]

{ #category : #'*Spec2-Transmission' }
SpAbstractPresenter >> hasPortNamed: aName [ 

	^ (self defineOutputPorts anySatisfy: [ :each | each name = aName ])
		or: [ self defineInputPorts anySatisfy: [ :each | each name = aName ] ]
]

{ #category : #'*Spec2-Transmission' }
SpAbstractPresenter >> inputPortDefault [

	"first input port will be default. 
	 this may be bad design and probably needs to be revisited later"
	^ self inputPorts first
]

{ #category : #'*Spec2-Transmission' }
SpAbstractPresenter >> inputPortNamed: aSymbol [

	^ self inputPorts detect: [ :each | each name = aSymbol ]
]

{ #category : #'*Spec2-Transmission' }
SpAbstractPresenter >> inputPorts [

	^ self defineInputPorts collect: [ :each | each presenter: self ]
]

{ #category : #'*Spec2-Transmission' }
SpAbstractPresenter >> newTransmission [

	^ SpTransmission new
]

{ #category : #'*Spec2-Transmission' }
SpAbstractPresenter >> outputPortDefault [

	"first output port will be default. 
	 this may be bad design and probably needs to be revisited later"
	^ self outputPorts first
]

{ #category : #'*Spec2-Transmission' }
SpAbstractPresenter >> outputPortNamed: aSymbol [

	^ self outputPorts detect: [ :each | each name = aSymbol ]
]

{ #category : #'*Spec2-Transmission' }
SpAbstractPresenter >> outputPorts [

	^ self defineOutputPorts collect: [ :each | each presenter: self ]
]

{ #category : #'*Spec2-Transmission' }
SpAbstractPresenter >> transmitDo: aBlock [
	
	self newTransmission
		from: self;
		do: aBlock;
		apply
]

{ #category : #'*Spec2-Transmission' }
SpAbstractPresenter >> transmitDo: aBlock transform: transformBlock [
	
	self newTransmission
		from: self;
		do: aBlock;
		transform: transformBlock;
		apply
]

{ #category : #'*Spec2-Transmission' }
SpAbstractPresenter >> transmitPort: aPortName do: aBlock [
	
	self newTransmission
		from: self port: aPortName;
		do: aBlock;
		apply
]

{ #category : #'*Spec2-Transmission' }
SpAbstractPresenter >> transmitPort: aPortName do: aBlock transform: transformBlock [
	
	self newTransmission
		from: self port: aPortName;
		do: aBlock;
		transform: transformBlock;
		apply
]

{ #category : #'*Spec2-Transmission' }
SpAbstractPresenter >> transmitTo: aPresenter [		
	
	^ self newTransmission
		from: self;
		to: aPresenter;
		apply
]

{ #category : #'*Spec2-Transmission' }
SpAbstractPresenter >> transmitTo: aPresenter transform: aValuable [
	
	^ self newTransmission
		from: self;
		to: aPresenter;
		transform: aValuable;
		apply
]

{ #category : #'*Spec2-Transmission' }
SpAbstractPresenter >> transmitTo: aPresenter transform: aValuable postTransmission: postValuable [
	
	^ self newTransmission
		from: self; 
		to: aPresenter;
		transform: aValuable;
		postTransmission: postValuable;
		apply
]
