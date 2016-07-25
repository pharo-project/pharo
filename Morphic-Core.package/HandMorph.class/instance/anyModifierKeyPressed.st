accessing
anyModifierKeyPressed
	^recentModifiers anyMask: 16r0E	"cmd | opt | ctrl"