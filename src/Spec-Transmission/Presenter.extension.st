Extension { #name : #Presenter }

{ #category : #'*Spec-Transmission' }
Presenter >> defineInputPorts [

	^ #()
]

{ #category : #'*Spec-Transmission' }
Presenter >> defineOutputPorts [

	^ #()
]

{ #category : #'*Spec-Transmission' }
Presenter >> inputPortDefault [

	"first input port will be default. 
	 this may be bad design and probably needs to be revisited later"
	^ self inputPorts first
]

{ #category : #'*Spec-Transmission' }
Presenter >> inputPortNamed: aSymbol [

	^ self inputPorts detect: [ :each | each name = aSymbol ]
]

{ #category : #'*Spec-Transmission' }
Presenter >> inputPorts [

	^ self defineInputPorts collect: [ :each | each presenter: self ]
]

{ #category : #'*Spec-Transmission' }
Presenter >> newTransmission [

	^ SpecTransmission new
]

{ #category : #'*Spec-Transmission' }
Presenter >> outputPortDefault [

	"first output port will be default. 
	 this may be bad design and probably needs to be revisited later"
	^ self outputPorts first
]

{ #category : #'*Spec-Transmission' }
Presenter >> outputPortNamed: aSymbol [

	^ self outputPorts detect: [ :each | each name = aSymbol ]
]

{ #category : #'*Spec-Transmission' }
Presenter >> outputPorts [

	^ self defineOutputPorts collect: [ :each | each presenter: self ]
]

{ #category : #'*Spec-Transmission' }
Presenter >> transmitDo: aBlock [
	
	self newTransmission
		from: self;
		do: aBlock;
		apply
]

{ #category : #'*Spec-Transmission' }
Presenter >> transmitDo: aBlock transform: transformBlock [
	
	self newTransmission
		from: self;
		do: aBlock;
		transform: transformBlock;
		apply
]

{ #category : #'*Spec-Transmission' }
Presenter >> transmitPort: aPortName do: aBlock [
	
	self newTransmission
		from: self port: aPortName;
		do: aBlock;
		apply
]

{ #category : #'*Spec-Transmission' }
Presenter >> transmitPort: aPortName do: aBlock transform: transformBlock [
	
	self newTransmission
		from: self port: aPortName;
		do: aBlock;
		transform: transformBlock;
		apply
]

{ #category : #'*Spec-Transmission' }
Presenter >> transmitTo: aPresenter [		
	
	^ self newTransmission
		from: self;
		to: aPresenter;
		apply
]

{ #category : #'*Spec-Transmission' }
Presenter >> transmitTo: aPresenter transform: aValuable [
	
	^ self newTransmission
		from: self;
		to: aPresenter;
		transform: aValuable;
		apply
]

{ #category : #'*Spec-Transmission' }
Presenter >> transmitTo: aPresenter transform: aValuable postTransmission: postValuable [
	
	^ self newTransmission
		from: self; 
		to: aPresenter;
		transform: aValuable;
		postTransmission: postValuable;
		apply
]
