accessing
model: anObject
    "Set my model and make me me a dependent of the given object."

    model ifNotNil: [model removeDependent: self].
    anObject ifNotNil: [anObject addDependent: self].
    model := anObject.