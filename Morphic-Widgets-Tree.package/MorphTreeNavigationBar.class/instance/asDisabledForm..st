accessing
asDisabledForm: aForm
	| disa |
	disa := aForm copy.
	(aForm colorsUsed reject: [:c | c = Color transparent])
		do: [:c | disa replaceColor: c  withColor: (c alphaMixed: 0.4 with: Color white)].
	^ disa
