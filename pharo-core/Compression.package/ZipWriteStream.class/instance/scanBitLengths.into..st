dynamic blocks
scanBitLengths: bits into: anArray
	"Scan the trees and determine the frequency of the bit lengths.
	For repeating codes, emit a repeat count."
	| lastValue lastCount value |
	bits size = 0 ifTrue:[^self].
	lastValue := bits at: 1.
	lastCount := 1.
	2 to: bits size do:[:i|
		value := bits at: i.
		value = lastValue 
			ifTrue:[lastCount := lastCount + 1]
			ifFalse:[self scanBitLength: lastValue repeatCount: lastCount into: anArray.
					lastValue := value.
					lastCount := 1]].
	self scanBitLength: lastValue repeatCount: lastCount into: anArray.