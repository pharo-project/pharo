accessing
at: aKey put: aValue
       "Replace the property value or pragma associated with aKey."
       | keyAlreadyExists |
       keyAlreadyExists := false.

       1 to: self basicSize do:
               [:i |
               | propertyOrPragma "<Association|Pragma>" |
               (propertyOrPragma := self basicAt: i) key == aKey ifTrue: [
                       keyAlreadyExists := true.
                       propertyOrPragma isVariableBinding
                               ifTrue: [propertyOrPragma value: aValue]
                               ifFalse: [self basicAt: i put: aValue]]].

       keyAlreadyExists ifFalse: [
               method propertyValueAt: aKey put: aValue ].

       ^ aValue
