backward compatibility
ownCategories
        "get all categories that are not obtained from traits. Order is random"

        | traitsCategories |

        traitsCategories := self subject traits
                inject: Set new
                into: [:set :trait |
                        set addAll: (trait organization protocolOrganizer protocols
                                reject: [ :eachProtocol |
                                        self subject methods anySatisfy: [ :eachMethod |
                                                eachMethod origin = self subject
                                                                and: [ eachMethod protocol = eachProtocol name ] ] ]).
                        set ].

        ^ ((self protocolOrganizer protocols collect: #name)
                copyWithoutAll: (traitsCategories collect: #name)) asArray.