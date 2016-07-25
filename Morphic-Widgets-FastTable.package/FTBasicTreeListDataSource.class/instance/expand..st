expanding-collapsing
expand: aNumber

	rootItems do: [ :each | each expandToDepth: aNumber ].