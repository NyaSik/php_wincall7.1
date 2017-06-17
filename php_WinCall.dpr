library php_WinCall;

uses
  System.Types,
  System.UITypes,
  Winapi.Windows,
  Winapi.Messages,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.Variants,
  System.TypInfo,
  System.SysConst,
  System.RTLConsts,
  System.Rtti,
  System.SysUtils,
  Classes,
  System.Hash,
  System.Math, LoadPhp, hzend_types;

var
  PTSRMLS_DC: Pointer;

function ZvalPDouble(z: zval): PDouble;
var
  x: double;
begin
  x := ZvalGetDouble(@z);
  Result := PDouble(@x);
end;

procedure Pre(str: string);

begin
  MessageBoxA(0, PAnsiChar(AnsiString(str)), '', 0)
end;

function IsClass(Address: Pointer): Boolean; assembler;
asm
  CMP     Address, Address.vmtSelfPtr
  JNZ     @False
  MOV     Result, True
  JMP     @Exit
@False:
  MOV     Result, False
@Exit:
end;

function IsObject(Address: Pointer): Boolean; assembler;
asm
  MOV     EAX, [Address]
  CMP     EAX, EAX.vmtSelfPtr
  JNZ     @False
  MOV     Result, True
  JMP     @Exit
@False:
  MOV     Result, False
@Exit:
end;

function PTValue(pp1: Pointer): TValue; overload;
begin
  if IsClass(pp1) then
    Result := TValue(TClass(pp1))
  else if IsObject(pp1) then
    Result := TValue(TObject(pp1))
  else
    Result := TValue(pp1);
end;

function PTValue(pp1: integer): TValue; overload;
begin
  Result := PTValue(Pointer(pp1));
end;

function GetTypeInfoStr(str, funcname: string): PTypeInfo;
begin
  str := str.ToLower.Trim;

  if str.IsEmpty or (str = 'void') then
    Result := nil
  else if str = 'pointer' then
    Result := TypeInfo(Pointer)
  else if str = 'char' then
    Result := TypeInfo(char)
  else if str = 'ansichar' then
    Result := TypeInfo(ansichar)
  else if str = 'widechar' then
    Result := TypeInfo(ansichar)
  else if (str = 'pwidechar') or (str = 'pchar') or (str = 'lpctstr') or
    (str = 'lpcwstr') then
    Result := TypeInfo(pwidechar)
  else if (str = 'pansichar') or (str = 'achar') or (str = 'lpcstr') then
    Result := TypeInfo(PAnsiChar)
  else if str = 'pansistring' then
    Result := TypeInfo(pansistring)
  else if str = 'unicodestring' then
    Result := TypeInfo(unicodestring)
  else if (str = 'fixedint') or (str = 'longint') then
    Result := TypeInfo(LongInt)
  else if (str = 'fixeduint') or (str = 'longword') or (str = 'dword') then
    Result := TypeInfo(LongWord)
  else if str = 'cpplongint' then
    Result := TypeInfo(cpplongint)
  else if str = 'cppulongint' then
    Result := TypeInfo(cppulongint)
  else if (str = 'int') or (str = 'integer') then
    Result := TypeInfo(integer)
  else if str = 'intptr' then
    Result := TypeInfo(intptr)
  else if str = 'bytebool' then
    Result := TypeInfo(bytebool)
  else if str = 'wordbool' then
    Result := TypeInfo(wordbool)
  else if str = 'longbool' then
    Result := TypeInfo(longbool)
  else if str = 'nativeint' then
    Result := TypeInfo(nativeint)
  else if str = 'nativeuint' then
    Result := TypeInfo(nativeuint)
  else if str = 'shortint' then
    Result := TypeInfo(shortint)
  else if str = 'smallint' then
    Result := TypeInfo(smallint)
  else if str = 'byte' then
    Result := TypeInfo(byte)
  else if str = 'word' then
    Result := TypeInfo(word)
  else if str = 'cardinal' then
    Result := TypeInfo(cardinal)
  else if str = 'int8' then
    Result := TypeInfo(int8)
  else if str = 'int16' then
    Result := TypeInfo(int16)
  else if str = 'int32' then
    Result := TypeInfo(int32)
  else if str = 'uint' then
    Result := TypeInfo(UINT)
  else if str = 'puint' then
    Result := TypeInfo(puint)
  else if str = 'ulong' then
    Result := TypeInfo(ulong)
  else if str = 'pulong' then
    Result := TypeInfo(pulong)
  else if str = 'plongint' then
    Result := TypeInfo(plongint)
  else if str = 'pinteger' then
    Result := TypeInfo(pinteger)
  else if str = 'plongword' then
    Result := TypeInfo(plongword)
  else if str = 'psmallint' then
    Result := TypeInfo(psmallint)
  else if str = 'pdouble' then
    Result := TypeInfo(PDouble)
  else if str = 'pshortint' then
    Result := TypeInfo(pshortint)
  else if (str = 'int_ptr') or (str = 'lparam') then
    Result := TypeInfo(int_ptr)
  else if (str = 'uint_ptr') or (str = 'wparam') then
    Result := TypeInfo(uint_ptr)
  else if str = 'long_ptr' then
    Result := TypeInfo(long_ptr)
  else if str = 'ulong_ptr' then
    Result := TypeInfo(ULONG_PTR)
  else if str = 'dword_ptr' then
    Result := TypeInfo(dword_ptr)
  else if str = 'handle_ptr' then
    Result := TypeInfo(handle_ptr)
  else if str = 'size_t' then
    Result := TypeInfo(size_t)
  else if str = 'ssize_t' then
    Result := TypeInfo(ssize_t)
  else if str = 'pint_ptr' then
    Result := TypeInfo(pint_ptr)
  else if str = 'puint_ptr' then
    Result := TypeInfo(puint_ptr)
  else if str = 'plong_ptr' then
    Result := TypeInfo(plong_ptr)
  else if str = 'pulong_ptr' then
    Result := TypeInfo(pulong_ptr)
  else if str = 'pdword_ptr' then
    Result := TypeInfo(pdword_ptr)
  else if str = 'psize_t' then
    Result := TypeInfo(psize_t)
  else if str = 'pssize_t' then
    Result := TypeInfo(pssize_t)
  else if str = 'uint8' then
    Result := TypeInfo(uint8)
  else if str = 'uint16' then
    Result := TypeInfo(uint16)
  else if str = 'uint32' then
    Result := TypeInfo(uint32)
  else if str = 'uintptr' then
    Result := TypeInfo(uintptr)
  else if (str = 'float') then
    Result := TypeInfo(single)
  else if (str = 'double') then
    Result := TypeInfo(double)
  else if str = 'shortstring' then
    Result := TypeInfo(shortstring)
  else if str = 'string' then
    Result := TypeInfo(string)
  else if str = 'wchar' then
    Result := TypeInfo(wchar)
  else if (str = 'astring') or (str = 'ansistring') then
    Result := TypeInfo(AnsiString)
  else if (str = 'wstring') or (str = 'widestring') then
    Result := TypeInfo(widestring)
  else if str = 'int64' then
    Result := TypeInfo(int64)
  else if str = 'uint64' then
    Result := TypeInfo(uint64)
  else if (str = 'bool') or (str = 'boolean') then
    Result := TypeInfo(Boolean)
  else if str = 'hwnd' then
    Result := TypeInfo(hWnd)
  else if str = 'hhook' then
    Result := TypeInfo(hhook)
  else if str = 'thandle' then
    Result := TypeInfo(thandle)
  else if (str = 'hdc') or (str = 'hmenu') or (str = 'hfont') or (str = 'hicon')
  then
    Result := TypeInfo(hdc)
  else
  begin
    zend_error((1 shl 8),
      PAnsiChar(AnsiString('Данный тип ''' + str + ''' не был найден.' + #13 +
      'CallFuncion: ' + funcname + #13 + 'Line: (' + zend_get_executed_lineno()
      .ToString)));

    Result := nil;
  end;
end;

function ParEx(p: string; value: pzval; i: byte; funcname: string): TValue;
var
  a: Pointer;
begin
  Result := nil;

  p := p.ToLower.Trim;

  if p = 'pointer' then
    Result := TValue.From<Pointer>(Pointer(ZvalGetInt(value)))
  else if p = 'char' then
    Result := TValue.From<char>(char(ZvalGetStringA(value)[1]))
  else if p = 'ansichar' then
    Result := TValue.From<ansichar>(ZvalGetStringA(value)[1])
  else if p = 'widechar' then
    Result := TValue.From<widechar>(ZvalGetString(value)[1])
  else if (p = 'pwidechar') or (p = 'pchar') or (p = 'lpctstr') or
    (p = 'lpcwstr') then
  begin
    a := StrNew(pwidechar(StringToOleStr(ZvalGetString(value))));
    TValue.Make(@a, TypeInfo(pwidechar), Result);
  end
  else if (p = 'pansichar') or (p = 'achar') or (p = 'lpcstr') then
  begin
    a := StrNew(PAnsiChar(AnsiString(ZvalGetString(value))));
    TValue.Make(@a, TypeInfo(PAnsiChar), Result);
  end
  else if p = 'pansistring' then
  begin
    a := pansistring(ZvalGetString(value));
    TValue.Make(@a, TypeInfo(pansistring), Result);
  end
  else if p = 'unicodestring' then
    Result := TValue.From<unicodestring>
      (StringToOleStr(StringToOleStr(ZvalGetString(value))))
  else if (p = 'fixedint') or (p = 'longint') then
    Result := TValue.From<LongInt>(ZvalGetInt(value))
  else if (p = 'fixeduint') or (p = 'longword') or (p = 'dword') then
    Result := TValue.From<LongWord>(ZvalGetInt(value))
  else if p = 'cpplongint' then
    Result := TValue.From<cpplongint>(ZvalGetInt(value))
  else if p = 'cppulongint' then
    Result := TValue.From<cppulongint>(ZvalGetInt(value))
  else if (p = 'int') or (p = 'integer') then
    Result := TValue.From<integer>(ZvalGetInt(value))
  else if p = 'intptr' then
    Result := TValue.From<intptr>(ZvalGetInt(value))
  else if p = 'bytebool' then
    Result := TValue.From<bytebool>(bytebool(ZvalGetInt(value)))
  else if p = 'wordbool' then
    Result := TValue.From<wordbool>(wordbool(ZvalGetInt(value)))
  else if p = 'longbool' then
    Result := TValue.From<longbool>(longbool(ZvalGetInt(value)))
  else if p = 'nativeint' then
    Result := TValue.From<nativeint>(ZvalGetInt(value))
  else if p = 'nativeuint' then
    Result := TValue.From<nativeuint>(ZvalGetInt(value))
  else if p = 'shortint' then
    Result := TValue.From<shortint>(ZvalGetInt(value))
  else if p = 'smallint' then
    Result := TValue.From<smallint>(ZvalGetInt(value))
  else if p = 'byte' then
    Result := TValue.From<byte>(ZvalGetInt(value))
  else if p = 'word' then
    Result := TValue.From<word>(ZvalGetInt(value))
  else if p = 'cardinal' then
    Result := TValue.From<cardinal>(ZvalGetInt(value))
  else if p = 'int8' then
    Result := TValue.From<int8>(ZvalGetInt(value))
  else if p = 'int16' then
    Result := TValue.From<int16>(ZvalGetInt(value))
  else if p = 'int32' then
    Result := TValue.From<int32>(ZvalGetInt(value))
  else if p = 'uint' then
    Result := TValue.From<UINT>(ZvalGetInt(value))
  else if p = 'puint' then
    Result := TValue.From<puint>(puint(ZvalGetInt(value)))
  else if p = 'ulong' then
    Result := TValue.From<ulong>(ZvalGetInt(value))
  else if p = 'pulong' then
  begin
    a := pulong(ZvalGetInt(value));
    TValue.Make(@a, TypeInfo(pulong), Result);
  end
  else if p = 'plongint' then
  begin
    a := plongint(ZvalGetInt(value));
    TValue.Make(@a, TypeInfo(plongint), Result);
  end
  else if p = 'pinteger' then
  begin
    a := pinteger(ZvalGetInt(value));
    TValue.Make(@a, TypeInfo(pinteger), Result);
  end
  else if p = 'plongword' then
  begin
    a := plongword(ZvalGetInt(value));
    TValue.Make(@a, TypeInfo(plongword), Result);
  end
  else if p = 'psmallint' then
  begin
    a := psmallint(ZvalGetInt(value));
    TValue.Make(@a, TypeInfo(psmallint), Result);
  end
  else if p = 'pdouble' then
  begin
    a := ZvalPDouble(value^);
    TValue.Make(@a, TypeInfo(PDouble), Result);
  end
  else if p = 'pshortint' then
  begin
    a := pshortint(pshortint(ZvalGetInt(value)));
    TValue.Make(@a, TypeInfo(pshortint), Result);
  end
  else if (p = 'int_ptr') or (p = 'lparam') then
    Result := TValue.From<int_ptr>(ZvalGetInt(value))
  else if (p = 'uint_ptr') or (p = 'wparam') then
    Result := TValue.From<uint_ptr>(ZvalGetInt(value))
  else if p = 'long_ptr' then
    Result := TValue.From<long_ptr>(ZvalGetInt(value))
  else if p = 'ulong_ptr' then
    Result := TValue.From<ULONG_PTR>(ZvalGetInt(value))
  else if p = 'dword_ptr' then
    Result := TValue.From<dword_ptr>(ZvalGetInt(value))
  else if p = 'handle_ptr' then
    Result := TValue.From<handle_ptr>(ZvalGetInt(value))
  else if p = 'size_t' then
    Result := TValue.From<size_t>(ZvalGetInt(value))
  else if p = 'ssize_t' then
    Result := TValue.From<ssize_t>(ZvalGetInt(value))
  else if p = 'pint_ptr' then
  begin
    a := pint_ptr(ZvalGetInt(value));
    TValue.Make(@a, TypeInfo(pint_ptr), Result);
  end
  else if p = 'puint_ptr' then
  begin
    a := puint_ptr(ZvalGetInt(value));
    TValue.Make(@a, TypeInfo(puint_ptr), Result);
  end
  else if p = 'plong_ptr' then
  begin
    a := plong_ptr(ZvalGetInt(value));
    TValue.Make(@a, TypeInfo(plong_ptr), Result);
  end
  else if p = 'pulong_ptr' then
  begin
    a := pulong_ptr(ZvalGetInt(value));
    TValue.Make(@a, TypeInfo(pulong_ptr), Result);
  end
  else if p = 'pdword_ptr' then
  begin
    a := pdword_ptr(ZvalGetInt(value));
    TValue.Make(@a, TypeInfo(pdword_ptr), Result);
  end
  else if p = 'psize_t' then
  begin
    a := psize_t(ZvalGetInt(value));
    TValue.Make(@a, TypeInfo(psize_t), Result);
  end
  else if p = 'pssize_t' then
  begin
    a := pssize_t(ZvalGetInt(value));
    TValue.Make(@a, TypeInfo(pssize_t), Result);
  end
  else if p = 'uint8' then
    Result := TValue.From<uint8>(ZvalGetInt(value))
  else if p = 'uint16' then
    Result := TValue.From<uint16>(ZvalGetInt(value))
  else if p = 'uint32' then
    Result := TValue.From<uint32>(ZvalGetInt(value))
  else if p = 'uintptr' then
    Result := TValue.From<uintptr>(ZvalGetInt(value))
  else if (p = 'float') or (p = 'single') then
  begin
    try
      FormatSettings.DecimalSeparator := '.';
      Result := TValue.From<single>(StrToFloat(ZvalGetString(value)))
    except
      on E: Exception do
        zend_error((1 shl 8),
          PAnsiChar(AnsiString('CallFuncion: ' + funcname + #13 + 'Line: (' +
          zend_get_executed_lineno().ToString + ') Args: ' + i.ToString + #13 +
          E.ClassName + ': ' + E.Message)));

    end;
  end
  else if (p = 'double') then
  begin
    try
      FormatSettings.DecimalSeparator := '.';
      Result := TValue.From<double>(StrToFloat(ZvalGetString(value)));
    except
      on E: Exception do
        zend_error((1 shl 8),
          PAnsiChar(AnsiString('CallFuncion: ' + funcname + #13 + 'Line: (' +
          zend_get_executed_lineno().ToString + ') Args: ' + i.ToString + #13 +
          E.ClassName + ': ' + E.Message)));

    end;
  end
  else if p = 'shortstring' then
    Result := TValue.From<shortstring>(shortstring(ZvalGetStringA(value)))
  else if p = 'string' then
    Result := string(ZvalGetStringA(value))
  else if p = 'wchar' then
    Result := TValue.From<wchar>(ZvalGetString(value)[1])
  else if (p = 'astring') or (p = 'ansistring') then
    Result := TValue.From<AnsiString>(ZvalGetStringA(value))
  else if (p = 'wstring') or (p = 'widestring') then
    Result := TValue.From<widestring>(ZvalGetString(value))
  else if p = 'int64' then
    Result := TValue.From<int64>(StrToInt64(string(ZvalGetStringA(value))))
  else if p = 'uint64' then
    Result := TValue.From<uint64>(StrToInt64(string(ZvalGetStringA(value))))
  else if (p = 'bool') or (p = 'boolean') then
    Result := TValue.From<Boolean>(ZvalGetBool(value))
  else if p = 'hwnd' then
    Result := TValue.From<hWnd>(ZvalGetInt(value))
  else if p = 'hhook' then
    Result := TValue.From<hhook>(ZvalGetInt(value))
  else if p = 'thandle' then
    Result := TValue.From<thandle>(ZvalGetInt(value))
  else if (p = 'hdc') or (p = 'hmenu') or (p = 'hfont') or (p = 'hicon') then
    Result := TValue.From<hdc>(ZvalGetInt(value))
  else
  begin

    zend_error((1 shl 8),
      PAnsiChar(AnsiString('Данный тип ''' + p +
      ''' не был объявлен в структуре поддерживаемых типов.' + #13 +
      'CallFuncion: ' + funcname + #13 + 'Line: (' + zend_get_executed_lineno()
      .ToString + ') Args: ' + i.ToString)));

    Result := nil;
  end;
end;

procedure TestSetRet(var return_value: pzval; Result: TValue);
begin
  if Result.IsEmpty then
  begin
    ZvalVAL(return_value);
    Exit;
  end;
  case Result.Kind of
    tkPointer:
      begin
        if (Result.TypeInfo.Name = 'PWideChar') or
          (Result.TypeInfo.Name = 'PChar') then
          ZvalVAL(return_value, Result.AsType<PChar>)
        else if (Result.TypeInfo.Name = 'PAnsiChar') or
          (Result.TypeInfo.Name = '_PAnsiChar') then
          ZvalVAL(return_value, Result.AsType<PAnsiChar>)
        else if Result.TypeInfo.Name = 'PAnsiString' then
          ZvalVAL(return_value, PAnsiChar(Result.AsType<pansistring>))
        else if Result.TypeInfo.Name = 'UnicodeString' then
          ZvalVAL(return_value, Result.AsType<unicodestring>)
        else
          ZvalVAL(return_value, integer(Result.GetReferenceToRawData));
      end;
    tkClass, tkClassRef:
      zend_error((1 shl 8), 'Попытка создать класс из WinCall');
    tkFloat:
      ZvalVAL(return_value, Result.AsExtended);
    tkInteger:
      ZvalVAL(return_value, integer(Result.AsInteger));
    tkInt64:
      ZvalVAL(return_value, integer(Result.AsInt64));
    tkEnumeration:
      if (Result.AsOrdinal >= Result.TypeData.MinValue) and
        (Result.AsOrdinal <= Result.TypeData.MaxValue) then
        ZvalVAL(return_value, GetEnumName(Result.TypeInfo, Result.AsOrdinal))
      else
        ZvalVAL(return_value, UTF8ToString(Result.TypeData^.NameList));
    tkString, tkWChar, tkLString, tkWString, tkUString:
      ZvalVAL(return_value, Result.ToString);
  else

    MessageBoxA(0, PAnsiChar(AnsiString('Realize Type Kind ' +
      GetEnumName(System.TypeInfo(TTypeKind), Ord(Result.Kind)) +
      '! (TestSetRet)')), 'TestSetRet', 0);

    ZvalVAL(return_value, Result.ToString);
  end;
end;

function LibCall(nameFunc: pzval; func: Pointer; RetType, args, Types: pzval;
  CallingConvention: TCallConv): TValue;
var
  argCount: integer;
  argList: TArray<TValue>;
  i: integer;

  tmp, tmp2: pzval;
  stre: string;
begin

  argCount := args.value.arr.nNumOfElements;
  SetLength(argList, argCount);
  stre := ZvalGetString(nameFunc);
  for i := 0 to argCount - 1 do
  begin
    tmp := zend_hash_index_findZval(args, i);
    tmp2 := zend_hash_index_findZval(Types, i);

    argList[i] := ParEx(string(ZvalGetStringA(tmp2)), tmp, i, stre);
  end;

  Result := System.Rtti.Invoke(func, argList, CallingConvention,
    GetTypeInfoStr(string(ZvalGetStringA(RetType)), stre));
end;

function LoadFunctionDll(DllName: PAnsiChar; var func: Pointer;
  funcname: PAnsiChar): byte;
var
  PHP5dll: thandle;
begin

  PHP5dll := GetModuleHandleA(DllName);
  if PHP5dll = 0 then
  begin
    PHP5dll := LoadLibraryA(DllName);
    if PHP5dll = 0 then
    begin
      if FileExists(String(DllName)) then
        Exit(2)
      else
        Exit(1);
    end;
  end;

  func := GetProcAddress(PHP5dll, funcname);
  if not assigned(func) then
  begin
    Pre('Not Load ''' + funcname + '''' + #13 + 'Dll:' + PHP5dll.ToString);
    Exit(3);
  end;

  Exit(0);
end;

function ZvalArgIdx(a: pzend_execute_data; i: integer): pzval;
begin
  Result := nil;
  if i in [0 .. a.this.u2.num_args - 1] then
  begin
    Result := pzval(nativeint(a) + ((i + 3) shl SizeOf(pzval)));

    if Result.u1.v._type = IS_REFERENCE then
      Result := @Result^.value.ref.Val;
  end

end;

procedure PHPLoadFunctionDll(execute_data: pzend_execute_data;
  return_value: pzval); cdecl;
var
  DllName, funcname, funcr: pzval;
  func: Pointer;
  nameFunc: PAnsiChar;

  f: Boolean;
  tmp: pzval;
begin

  ZvalVAL(return_value, 0);

  DllName := ZvalArgIdx(execute_data, 0);
  funcname := ZvalArgIdx(execute_data, 1);

  f := True;
  func := nil;

  if funcname^.u1.v._type = IS_ARRAY then
  begin
    if funcname^.value.arr.nNumOfElements = 2 then
    begin
      tmp := zend_hash_index_findZval(funcname, 1);
      _convert_to_string(tmp, nil, 0);
      nameFunc := tmp^.value.str.Val;

    end
    else
    begin

      f := False;
      nameFunc := '...';
      if funcname^.value.arr.nNumOfElements >= 1 then
      begin

        tmp := zend_hash_index_findZval(funcname, 0);
        nameFunc := PAnsiChar(ZvalGetStringA(tmp));
      end;

      zend_error((1 shl 8),
        PAnsiChar(AnsiString(ExtractFileName(string(ZvalGetString(DllName))) +
        ':' + string(nameFunc) +
        ' Допустимиый вид массива array(''NameFunction'', ''NameFunction@12'')')
        ));
    end;

  end
  else
  begin
    nameFunc := PAnsiChar(ZvalGetStringA(funcname));
  end;

  if f then
  begin
    ZvalVAL(return_value,

      LoadFunctionDll(PAnsiChar(ZvalGetStringA(DllName)), func, nameFunc)
      .ToString + '||' + nativeint(func).ToString);

  end
  else
    ZvalVAL(return_value, -1);

end;

procedure PHPLibCall(execute_data: pzend_execute_data;
  return_value: pzval); cdecl;
var
  nameFunc, funcr, args, CC, Types, RetType: pzval;
  CallingConvention: TCallConv;
begin
  ZvalVAL(return_value, 0);

  CallingConvention := ccStdCall;

  nameFunc := ZvalArgIdx(execute_data, 0);
  funcr := ZvalArgIdx(execute_data, 1);
  RetType := ZvalArgIdx(execute_data, 2);
  args := ZvalArgIdx(execute_data, 3);
  Types := ZvalArgIdx(execute_data, 4);
  CC := ZvalArgIdx(execute_data, 5);

  case ZvalGetInt(CC) of
    0:
      CallingConvention := ccStdCall;
    1:
      CallingConvention := ccPascal;
    2:
      CallingConvention := ccCdecl;
    3:
      CallingConvention := ccReg;
    4:
      CallingConvention := ccSafeCall;
  end;

  TestSetRet(return_value, LibCall(nameFunc, Pointer(ZvalGetInt(funcr)),
    RetType, args, Types, CallingConvention));

end;

function HRESULTStr(h: HRESULT): PChar;
begin
  FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_SYSTEM,
    nil, h, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), @Result, 0, nil);
end;

procedure PHPGetLastError(execute_data: pzend_execute_data;
  return_value: pzval); cdecl;
begin
  ZvalVAL(return_value, PAnsiChar(AnsiString(HRESULTStr(GetLastError))));
end;

var
  moduleEntry: _zend_module_entry;
  module_entry_table: array of _zend_function_entry;

procedure AddFuction(Name: PAnsiChar; CallBackFunc: Pointer);
var
  i: integer;
begin
  if High(module_entry_table) = -1 then
    SetLength(module_entry_table, 1);

  SetLength(module_entry_table, Length(module_entry_table) + 1);
  i := High(module_entry_table) - 1;
  module_entry_table[i].fname := Name;
  module_entry_table[i].arg_info := nil;
  module_entry_table[i].handler := CallBackFunc;
end;

procedure evalCode(Code: string);
begin
  zend_eval_string('<?php pre('' OK '');', nil, 'eval code');
end;

function rinit(_type: integer; module_number: integer; TSRMLS_DC: Pointer)
  : integer; cdecl;
begin
  PTSRMLS_DC := TSRMLS_DC;

  Result := SUCCESS;

end;

function ZEND_MODULE_BUILD_ID: AnsiString;
begin
  Result := 'API20160303,TS,VC14';
end;

function get_module: p_zend_module_entry; cdecl;
begin
  moduleEntry.Size := SizeOf(_zend_module_entry);
  moduleEntry.zend_api := ZEND_MODULE_API_NO;
  moduleEntry.build_id := StrNew(PAnsiChar(ZEND_MODULE_BUILD_ID));
  moduleEntry.request_startup_func := @rinit;
  moduleEntry.Name := 'WinCall';
  Result := @moduleEntry;

  AddFuction('PHPLoadFunctionDll', @PHPLoadFunctionDll);
  AddFuction('PHPLibCall', @PHPLibCall);
  AddFuction('PHPGetLastError', @PHPGetLastError);

  moduleEntry.functions := @module_entry_table[0];

  Result := @moduleEntry;
end;

exports get_module;

end.
