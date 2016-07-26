actions examples
browserWithToolbar
	<glmBrowser: 'Toolbar' input: '''Browser with toolbar'''>
	"self new browserWithToolbar openOn: 'Browser with toolbar'"

	| wrapper |
	wrapper := GLMWrapper new.
	wrapper act: [:f | f inspect] icon: GLMUIThemeExtraIcons glamorousInspect on: $i entitled: 'Inspect with icon'.
	wrapper act: [:f | f inspect] on: $i entitled: 'Inspect without icon'.
	wrapper show: [:a | a text ].
	^ wrapper