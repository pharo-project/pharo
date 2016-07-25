geometry
fontMetricsProvider: aTextLayout
	| font |
	font := self span getAttribute: TxFontAttribute.  
	^ aTextLayout  fontMetricsProviderFor: font.
