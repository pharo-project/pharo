testing
isBold
	^(simulatedEmphasis == nil and:[self face isBold])
		or:[self isSimulatedBold]