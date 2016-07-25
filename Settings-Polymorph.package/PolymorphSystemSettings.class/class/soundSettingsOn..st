sound
soundSettingsOn: aBuilder
	(aBuilder setting: #soundEnabled)
		label: 'Sound'; 
		target: SoundSystem;
		noOrdering;
		with: [
			(aBuilder setting: #useThemeSounds)
				label: 'Use theme sounds';
				target: SoundTheme; 
				with: [
					(aBuilder pickOne: #soundThemeClass)
						label: 'Sound theme' ;
						target: self;
						domainValues: self soundThemeClassChoices]]