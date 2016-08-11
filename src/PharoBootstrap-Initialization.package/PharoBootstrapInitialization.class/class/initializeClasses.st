class-initialization
initializeClasses
	" #classesInitializationOrder method is generated through a jenkins job 
	with the help of the ClassInitializationSorter class (SH Pharo/Bootstrap)."
	"see  https://ci.inria.fr/pharo/view/Pharo bootstrap/job/60-Bootstrap-Cargo-LoadInstructions"
	self runInitializeOnClasses: self classesInitializationOrder.
	UIManager default inform: 'initialization done!'