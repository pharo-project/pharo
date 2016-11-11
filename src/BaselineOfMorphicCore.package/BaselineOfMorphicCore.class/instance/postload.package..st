actions
postload: loader package: packageSpec

	| world hand |

	IdentityTransform initialize.

	EventSensorConstants initialize.

	RealEstateAgent initialize.
	WorldState initialize.
	Morph initialize.
	HandMorph initialize.

	Cursor initTarget.

	world := WorldMorph new.
	world instVarNamed: #worldState put: WorldState new.
	hand := HandMorph new.
	world addHand: hand.
	world activeHand.

	Smalltalk at: #World put: world.
	Smalltalk at: #ActiveWorld put: world.
	Smalltalk at: #ActiveHand put: hand.
	Smalltalk at: #ActiveEvent put: nil.

	world viewBox: Display boundingBox.

	Smalltalk at: #Sensor put: InputEventSensor basicNew.
	InputEventSensor installEventSensorFramework.
	InputEventSensor installMouseDecodeTable.

	(Smalltalk at: #Sensor) flushAllButDandDEvents.
	world world handsDo: [:h | h initForEvents].
	world borderWidth: 0.

	MorphicCoreUIManager new spawnNewProcess.
	Display newDepth: 32.
	world displayWorldSafely.
	UIManager default: MorphicCoreUIManager new.
	world displayWorldSafely.
	UIManager default uiProcess resume.

	World cleanseOtherworldlySteppers.
	ExternalDropHandler resetRegisteredHandlers.

	Display newDepth: 32.

	(world instVarNamed: #worldState) canvas: nil.

	(Smalltalk at: #ActiveHand) instVarNamed: #targetOffset put: 0@0.

	WorldMorph initialize.
	ExternalDropHandler initialize.
	PasteUpMorph initialize.
	DefaultExternalDropHandler initialize.
	
	WorldMorph recompile.
