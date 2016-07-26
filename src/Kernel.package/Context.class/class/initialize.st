simulation
initialize

	"A unique object to be returned when a primitive fails during simulation"
	PrimitiveFailToken := Object new.
	self initializeTryNamedPrimitiveTemplateMethod.
	SpecialPrimitiveSimulators := nil.
	QuickStep := nil.