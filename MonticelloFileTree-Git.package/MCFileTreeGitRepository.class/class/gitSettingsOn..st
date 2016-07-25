settings
gitSettingsOn: aBuilder
	"The group for such settings is #monticello, even if it is labelled Monticello."

	<systemsettings>
	(aBuilder setting: #gitCommand)
		type: #File;
		label: 'Git command path' translated;
		parent: #monticello;
		target: self;
		description:
			'The path to the git command if not in the default external command path'
				translated