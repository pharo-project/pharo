private
morphForItemAt: entryReference

	^ [ (self itemForEntryReference: entryReference) asMorph ]
			on: NotFound do: [:error | error asString asMorph ] "Enforce... remove it?"