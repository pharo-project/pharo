recompilation
decompileThenRecompileClass: aClass
	{aClass.
	aClass class}
		do: [ :class | 
			class localSelectors
				do: [ :aMethod | self recompile: aMethod from: class ]
				displayingProgress: [ :aMethod | 'Decompiling ' , class name , '>>#' , aMethod asString ] ]