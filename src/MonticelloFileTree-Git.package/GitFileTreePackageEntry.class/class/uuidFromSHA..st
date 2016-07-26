utility
uuidFromSHA: cid
	"Return an adequate uuid from a git commit ID."

	^ UUID nilUUID
		in: [ :uuid |
			1 to: 16 do: [ :i | uuid at: i put: (cid at: i) ].
			uuid at: 9 put: (16r80 bitOr: ((uuid at: 9) bitAnd: 16r3F)).	"Type"
			uuid at: 7 put: (16r50 bitOr: ((uuid at: 7) bitAnd: 16r0F)).	"Version 5 : SHA-1 Namespace (git)."
			uuid ]