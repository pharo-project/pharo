Each rule should define a #basicCheck: method which accepts an entity to be validated by the rule.

If the entity violates the rule, method should ruturn "true", otherwise - "false".

It is recommended to reuse #basicCheck: functionality in #checkClass: and #checkMethod: