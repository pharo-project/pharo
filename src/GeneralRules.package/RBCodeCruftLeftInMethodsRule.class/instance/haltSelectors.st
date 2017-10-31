private
haltSelectors
	| objectConvenience miscellaneous |
	objectConvenience := Object allSelectorsInProtocol: #halting.
	miscellaneous := #(inspectOnce).
	^ objectConvenience, miscellaneous