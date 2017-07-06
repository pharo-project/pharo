*NautilusRefactoring
renameMethodAllItem: aBuilder

	^ (aBuilder item: #'Rename method (all)')
			keyText: 'r, m' if: Nautilus useOldStyleKeys not;
			keyText: 'r' if: Nautilus useOldStyleKeys;
			action: [ | target |
				target := aBuilder model.
				"scroll := target methodWidget vScrollValue."
				target refactor renameMethodFor: target selectedMethod originMethod.
				"target methodWidget vScrollValue: scroll"
				 ].