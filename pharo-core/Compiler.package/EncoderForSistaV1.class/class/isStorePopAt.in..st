instruction stream support
isStorePopAt: pc in: method
	"Answer whether the bytecode at pc is a store or store-pop."

	"	200-207	11001 iii						Pop and Store Receiver Variable #iii
		208-215	11010 iii						Pop and Store Temporary Variable #iii
	 *	224		11100000	aaaaaaaa			Extend A (Ext A = Ext A prev * 256 + Ext A)
	 *	240		11110000	iiiiiiii				Pop and Store Receiver Variable #iiiiiii (+ Extend A * 256)
	 *	241		11110001	iiiiiiii				Pop and Store Literal Variable #iiiiiiii (+ Extend A * 256)
		242		11110010	iiiiiiii				Pop and Store Temporary Variable #iiiiiiii

		253		11111101 	kkkkkkkk	jjjjjjjj	Pop and Store Temp At kkkkkkkk In Temp Vector At: jjjjjjjj"

	| byte |
	byte := self nonExtensionBytecodeAt: pc in: method.
	^byte >= 200
	  and: [byte <= 215
		 or: [(byte between: 240 and: 242)
		 or: [byte = 253]]]