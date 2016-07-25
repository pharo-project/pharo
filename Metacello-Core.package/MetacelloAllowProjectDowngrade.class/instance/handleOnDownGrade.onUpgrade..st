handlers
handleOnDownGrade: onDownGradeBlock onUpgrade: onUpgradeBlock
    ^ onDownGradeBlock cull: self cull: self existingProjectRegistration cull: self newProjectRegistration