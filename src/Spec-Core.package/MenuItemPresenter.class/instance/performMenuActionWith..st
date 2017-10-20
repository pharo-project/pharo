api
performMenuActionWith: aMenuItem
	| en |

	en := self enabled.
	en isBlock ifTrue: [ en := en value ].
	en ifFalse: [ ^ self ].

	action value cull: aMenuItem