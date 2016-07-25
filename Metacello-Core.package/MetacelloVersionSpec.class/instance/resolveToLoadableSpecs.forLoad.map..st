loading
resolveToLoadableSpecs: required forLoad: forLoad map: packageMap
  | reqd allReqd map newReqd spec |
  reqd := required copy.
  allReqd := Set new.
  map := self packages map.
  [ reqd isEmpty ]
    whileFalse: [ 
      newReqd := Set new.
      reqd
        do: [ :req | 
          (self
            resolveToLoadableSpec: req
            forLoad: forLoad
            forMap: map
            packages: packageMap)
            do: [ :loadableSpec | 
              newReqd addAll: loadableSpec requires.
              newReqd addAll: loadableSpec includes ] ].
      allReqd addAll: reqd.
      newReqd removeAllFoundIn: allReqd.
      reqd := newReqd ].
  packageMap keys
    do: [ :pkgName | 
      (spec := (packageMap at: pkgName) resolveToLoadableSpec) == nil
        ifTrue: [ packageMap removeKey: pkgName ]
        ifFalse: [ packageMap at: pkgName put: (packageMap at: pkgName) resolveToLoadableSpec ] ]