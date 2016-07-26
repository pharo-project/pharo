language methods
nextPutValue: ascii toStream: aStream withShiftSequenceIfNeededForTextConverterState: state

	^ self traditionalCharsetClass nextPutValue: ascii toStream: aStream withShiftSequenceIfNeededForTextConverterState: state.