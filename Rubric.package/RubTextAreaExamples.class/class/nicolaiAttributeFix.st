examples
nicolaiAttributeFix
	<example>
	"self nicolaiAttributeFix"
	| font1 font2 tMorph |
	font1 := TextFontReference toFont: (StrikeFont familyName: 'Bitmap DejaVu Sans' size: 11).
	font2 := TextFontReference toFont: (LogicalFont familyName: 'Source Sans Pro' pointSize: 10).
	{font1.
	font2}
		do: [ :f | 
			| t1 t2 t3 t4 t5 t6 |
			tMorph := RubEditingArea new.
			t1 := 'normal\' withCRs asText addAttribute: f.
			t2 := 'bold\' withCRs asText
				addAttribute: f;
				addAttribute: TextEmphasis bold.
			t3 := 'underline\' withCRs asText
				addAttribute: f;
				addAttribute: TextEmphasis underlined.
			t4 := 'struck out\' withCRs asText
				addAttribute: f;
				addAttribute: TextEmphasis struckOut.
			t5 := 'italic\' withCRs asText
				addAttribute: f;
				addAttribute: TextEmphasis italic.
			t6 := 'narrow\' withCRs asText
				addAttribute: f;
				addAttribute: TextEmphasis narrow.
			tMorph updateTextWith: t1 , t2 , t3 , t4 , t5 , t6.
			tMorph openInWorld ]