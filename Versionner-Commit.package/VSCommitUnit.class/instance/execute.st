executing
execute
	self validate.
	"First commit packages"
	self packageActions do: [ :each | each executeOn: self ].
	"Now we can generate new versions and commit configs"
	self projectActions do: [ :each | each executeOn: self ]