origins
preferences
	"http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html"

	^ self directoryFromEnvVariableNamed: 'XDG_CONFIG_HOME' or: [ self home / '.config' ]