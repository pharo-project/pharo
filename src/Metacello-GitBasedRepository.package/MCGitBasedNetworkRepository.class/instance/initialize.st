initialize-release
initialize
    super initialize.
    directory := nil.	"we'll lazily create the directory instance"
    self readonly: true