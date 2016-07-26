origins
cache
	"http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html"

	^ self directoryFromEnvVariableNamed: 'XDG_CACHE_HOME' or: [ self home / '.cache' ]