fileIn/out
setEncoderForSourceCodeNamed: streamName

	| l |
	l := streamName asLowercase.
	((l endsWith: 'cs') or: [
		(l endsWith: 'st') or: [
			(l endsWith: ('st.gz')) or: [
				(l endsWith: ('st.gz'))]]]) ifTrue: [
					self converter: MacRomanTextConverter new.
					^ self.
	].
	self converter: UTF8TextConverter new.
