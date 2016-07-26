initialization
initialize
	super initialize.
	privacy := [ Privacy sendDiagnosticsAndUsageData ].
	collectors := GTEventCollectors new.
	delivery := GTEventDelivery new.
	delivery recorder: self.
	packing := GTEventPacking default.
	self canSendDiagnosticsAndUsageData 
			ifTrue: [ delivery offerPermission ]
			ifFalse: [ delivery withdrawPermission ]