private
usersMethod
	methodHolder value methodClass ifNotNil: [ :class | self model browseClassRefsOf: class ]