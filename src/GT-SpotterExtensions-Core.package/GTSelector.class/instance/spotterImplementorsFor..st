ui
spotterImplementorsFor: aStep
	<spotterOrder: 40>
	^ aStep listProcessor
		title: 'Implementors';
		filter: GTFilterSubstring item: [ :filter :context | self implementors do: filter ];
		keyBinding: $m meta