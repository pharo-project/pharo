accessing
openGlobalSpotterDebug

	GTSpotterMorph new
		extent: (World width / 2.25 @ (World height / 1.25)) asIntegerPoint;
		spotterModel: (GTSpotter new
			beDebugging;
			yourself);
		openInWindowLabeled: 'Spotter Debug Mode';
		showPreview