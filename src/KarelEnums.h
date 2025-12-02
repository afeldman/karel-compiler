// Manual enum definitions for BNFC C backend
// These are copied from Absyn.h to make them accessible in C++

#ifndef KAREL_ENUMS_H
#define KAREL_ENUMS_H

// Statement types
enum {
    is_SAttach, is_SRelease, is_SAbort, is_SAbortP, is_SAssign, 
    is_SCloseFile, is_SCloseHand, is_SDelay, is_SConnect, is_SDisable, 
    is_SDisconectT, is_SEnable, is_SOpenHand, is_SPurge, is_SRelax, 
    is_SSignal, is_SCancel, is_SCancelFile, is_SCancelGroup, is_SCallparamExp, 
    is_SForTo, is_SForDownTo, is_SGoTo, is_SGo_To, is_SHold, is_SHoleGroup, 
    is_SIfThen, is_SIfThenElse, is_SPause, is_SPauseProgram, is_SPulse, 
    is_SOpenFile, is_SReadIdent, is_SRead, is_SReturn, is_SReturnExp, 
    is_SUsing, is_SUnHold, is_SUnHoldExt, is_SStop, is_SStopExt, 
    is_SSelect, is_SSelectElse, is_SRepeat, is_SResum, is_SResumGroup, 
    is_SWait, is_SWhile, is_SWrite, is_SCondition, is_SConditionWith, 
    is_SMove, is_SMoveLinear, is_SMoveJoint, is_SMoveCircular, 
    is_SGetVar, is_SSetVar, is_SGetPort, is_SSetPort, 
    is_SGetReg, is_SSetReg, is_SGetTPE, is_SSetTPE, is_SClearTPE, 
    is_SGetJPos, is_SSetJPos, is_SGetPosReg, is_SSetPosReg, 
    is_SGetPosTPE, is_SSetPosTPE, is_SMsg, is_SActScreen, is_SDeactScreen
};

// Expression types  
enum {
    is_EBrack = 1000,  // Start at 1000 to avoid conflicts
    is_EEqual, is_ENEqual, is_ELess, is_ELeq, is_Egret, is_Egeq, is_Esp,
    is_EAdd, is_ESub, is_EOR, is_EPlus, is_EMinus, is_EMul, is_EAdiv, 
    is_EAnd, is_EMOD, is_EDiv, is_ENot, is_EDot, is_EAt, is_EHash, 
    is_EIdent, is_EQString, is_EDouble, is_EInt, 
    is_EABS, is_EACOS, is_EASIN, is_EATAN2, is_ECOS, is_ESin, is_ETan, 
    is_ESqrt, is_ELn, is_EExp, is_ETrunc, is_ERound, is_EStrlen, is_ESubstr, 
    is_EChr, is_EOrd, is_EUninitd, is_ECurPos, is_ECurJPos, is_EPosToJ, is_EJToPos
};

#endif // KAREL_ENUMS_H
