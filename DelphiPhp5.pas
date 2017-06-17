unit DelphiPhp5;

interface

uses
  // System.Types,
  // System.UITypes,
  Winapi.Windows,
  // Winapi.Messages,
  // System.Generics.Defaults,
  // System.Generics.Collections,
  // System.Variants,
  // System.TypInfo,
  // System.SysConst,
  // System.RTLConsts,
  // System.Rtti,
  System.SysUtils,
  // Classes,
  // System.Hash,
  System.Math;

const
  DllPHP = 'php5ts.dll';
  SUCCESS = 0;
  FAILURE = -1;
  IS_NULL = 0;
  IS_LONG = 1;
  IS_DOUBLE = 2;
  IS_BOOL = 3;
  IS_ARRAY = 4;
  IS_OBJECT = 5;
  IS_STRING = 6;
  IS_RESOURCE = 7;
  IS_CONSTANT = 8;
  IS_CONSTANT_AST = 9;
  IS_CALLABLE = 10;
  ZEND_MODULE_API_NO = 20090626;
  HASH_UPDATE = 1 SHL 0;
  HASH_ADD = 1 SHL 1;
  HASH_NEXT_INSERT = 1 SHL 2;

  HASH_DEL_KEY = 0;
  HASH_DEL_INDEX = 1;
  HASH_DEL_KEY_QUICK = 2;

type
  PPBucket = ^PBucket;
  PBucket = ^TBucket;

  TBucket = record
    h: ulong;
    nKeyLength: uint;
    pData: pointer;
    pDataPtr: pointer;
    pListNext: PBucket;
    pListLast: PBucket;
    pNext: PBucket;
    pLast: PBucket;
    arKey: array [0 .. 1] of AnsiChar;
  end;

  PHashTable = ^THashTable;

  THashTable = record
    nTableSize: uint;
    nTableMask: uint;
    nNumOfElements: uint;
    nNextFreeElement: ulong;
    pInternalPointer: PBucket;
    pListHead: PBucket;
    pListTail: PBucket;
    arBuckets: PPBucket;
    pDestructor: pointer;
    persistent: boolean;
    nApplyCount: byte;
    bApplyProtection: boolean;
  end;

  p_zend_module_entry = ^_zend_module_entry;

  _zend_module_entry = record
    size: word;
    zend_api: dword;
    zend_debug: byte;
    zts: byte;
    ini_entry: pointer;
    deps: pointer;
    name: PAnsiChar;
    functions: pointer;
    module_startup_func: pointer;
    module_shutdown_func: pointer;
    request_startup_func: pointer;
    request_shutdown_func: pointer;
    info_func: pointer;
    version: PAnsiChar;
    globals_size: size_t;
    globals_id_ptr: pointer;
    globals_ctor: pointer;
    globals_dtor: pointer;
    post_deactivate_func: pointer;
    module_started: integer;
    _type: byte;
    handle: pointer;
    module_number: integer;
    build_id: PAnsiChar;
  end;

  P_zend_arg_info = ^_zend_arg_info;

  _zend_arg_info = record
    name: PAnsiChar;
    name_len: uint;
    class_name: PAnsiChar;
    class_name_len: uint;
    array_type_hint: boolean;
    allow_null: boolean;
    pass_by_reference: boolean;
    return_reference: boolean;
    required_num_args: integer;
  end;

  PZend_class_entry = ^Tzend_class_entry;

  zend_object_value = record
    handle: integer;
    handlers: pointer;
  end;

  _zend_function_entry = record
    fname: PAnsiChar;
    handler: pointer;
    arg_info: P_zend_arg_info;
    num_args: uint;
    flags: uint;
  end;

  Pzvalue_value = ^zvalue_value;

  zvalue_value = record
    case integer of
      0:
        (lval: Longint;);
      1:
        (dval: double;);
      2:
        (str: record val: PAnsiChar;
          len: Longint;
        end;);
      3:
        (ht: PHashTable;);
      4:
        (obj: zend_object_value;);
  end;

  pppzval = ^ppzval;
  ppzval = ^pzval;
  pzval = ^zval;
  pzval_array_ex = array of pzval;

  zval = record
    value: zvalue_value;
    refcount__gc: uint;
    _type: byte;
    is_ref__gc: byte;
  end;

  Tzend_class_entry = record
    _type: AnsiChar;
    name: PAnsiChar;
    name_length: uint;
    parent: PZend_class_entry;
    refcount: integer;
    constants_updated: boolean;
    ce_flags: uint;

    function_table: THashTable;
    default_properties: THashTable;
    properties_info: THashTable;
    default_static_members: THashTable;

    static_members: PHashTable;
    constants_table: THashTable;
    builtin_functions: pointer;

    _constructor: pointer;
    _destructor: pointer;
    clone: pointer;
    __get: pointer;
    __set: pointer;
    // {$IFDEF PHP510}
    __unset: pointer;
    __isset: pointer;
    // {$ENDIF}
    __call: pointer;
    // {$IFDEF PHP530}
    __callstatic: pointer;
    // {$ENDIF}
    // {$IFDEF PHP520}
    __tostring: pointer;
    // {$ENDIF}
    // {$IFDEF PHP510}
    serialize_func: pointer;
    unserialize_func: pointer;
    // {$ENDIF}
    iterator_funcs: pointer;

    create_object: pointer;
    get_iterator: pointer;
    interface_gets_implemented: pointer;

    get_static_method: pointer;

    serialize: pointer;
    unserialize: pointer;

    interfaces: pointer;
    num_interfaces: uint;

    filename: PAnsiChar;
    line_start: uint;
    line_end: uint;
    doc_comment: PAnsiChar;
    doc_comment_len: uint;

    module: pointer;
  end;

  PZendObject = ^TZendObject;

  _zend_object = record
    gc: pointer;
    handle: uint32;
    ce: PZend_class_entry;
    handlers: pointer;
    properties: PHashTable;
    properties_table: array [0 .. 0] of zval;
  end;

  zend_object = _zend_object;
  TZendObject = _zend_object;

  Pzend_executor_globals = ^zend_executor_globals;

  zend_executor_globals = record
    return_value_ptr_ptr: ppzval;

    uninitialized_zval: zval;
    uninitialized_zval_ptr: pzval;

    error_zval: zval;
    error_zval_ptr: pzval;

    function_state_ptr: pointer;
    arg_types_stack: pointer;

    // symbol table cache
    symtable_cache: array [0 .. 31] of PHashTable;
    symtable_cache_limit: ^PHashTable;
    symtable_cache_ptr: ^PHashTable;

    opline_ptr: pointer;

    active_symbol_table: PHashTable;
    symbol_table: THashTable; // main symbol table

    included_files: THashTable; // files already included */

    bailout: pointer;

    error_reporting: integer;
    orig_error_reporting: integer;
    exit_status: integer;

    active_op_array: pointer;

    function_table: PHashTable; // function symbol table */
    class_table: PHashTable; // class table
    zend_constants: PHashTable; // constants table */

    scope: pointer;
    _this: pzval;

    precision: Longint;

    ticks_count: integer;

    in_execution: boolean;
{$IFDEF PHP5}
    in_autoload: PHashTable;
{$IFDEF PHP510}
    autoload_func: pointer;
{$ENDIF}
{$ENDIF}
{$IFDEF PHP4}
    bailout_set: zend_bool;
{$ENDIF}
    full_tables_cleanup: boolean;
{$IFDEF PHP5}
    ze1_compatibility_mode: zend_bool;
{$ENDIF}
    // for extended information support */
    no_extensions: boolean;

    timed_out: boolean;

    regular_list: THashTable;
    persistent_list: THashTable;

    argument_stack: pointer;

    free_op1, free_op2: pzval;
    unary_op: pointer;
    binary_op: pointer;

    garbage: array [0 .. 1] of pzval;
    garbage_ptr: integer;

    user_error_handler_error_reporting: integer;
    user_error_handler: pzval;
    user_exception_handler: pzval;
    user_error_handlers_error_reporting: pointer;

    user_error_handlers: pointer;
    user_exception_handlers: pointer;

    // * timeout support */
    timeout_seconds: integer;
    lambda_count: integer;
    ini_directives: PHashTable;

    objects_store: pointer;
    exception: pzval;
    opline_before_exception: pointer;
    current_execute_data: pointer;

    current_module: pointer;

    std_property_info: pointer;

    // * locale stuff */

{$IFNDEF PHP510}
    float_separator: AnsiChar;
{$ENDIF}
    reserved: array [0 .. 3] of pointer;
  end;

var
  PHP5dll: THandle = 0;
  ZEND_MODULE_BUILD_ID: PAnsiChar = 'API20090626,TS,VC9';

  zend_get_parameters_ex: function(param_count: integer; Args: ppzval): integer;
cdecl varargs;
_estrndup:
function(s: PAnsiChar; len: Cardinal; zend_filename: PAnsiChar;
  zend_lineno: uint; zend_orig_filename: PAnsiChar; zend_orig_line_no: uint)
  : PAnsiChar; cdecl;
zend_wrong_param_count:
procedure(TSRMLS_D: pointer); cdecl;

zend_hash_copy:
procedure(target: PHashTable; source: PHashTable; pCopyConstructor: pointer;
  tmp: pointer; size: uint); cdecl;

zend_hash_func:
function(arKey: PAnsiChar; nKeyLength: uint): Longint; cdecl;
_zend_hash_quick_add_or_update:
function(ht: PHashTable; arKey: PAnsiChar; nKeyLength: uint; h: uint;
  out pData: pzval; nDataSize: uint; pDest: PPointer; flag: integer)
  : integer; cdecl;
zend_hash_exists:
function(ht: PHashTable; arKey: PAnsiChar; nKeyLength: uint): integer; cdecl;
zend_hash_index_exists:
function(ht: PHashTable; h: ulong): integer; cdecl;
zend_hash_del_key_or_index:
function(ht: PHashTable; arKey: PAnsiChar; nKeyLength: uint; y: ulong;
  flag: integer): integer; cdecl;
zend_hash_quick_find:
function(const ht: PHashTable; arKey: PAnsiChar; nKeyLength: uint; h: ulong;
  out pData: ppzval): integer; cdecl;

zend_hash_add_or_update:
function(ht: PHashTable; arKey: PAnsiChar; nKeyLength: uint; pData: pointer;
  nDataSize: uint; pDest: pointer; flag: integer): integer; cdecl;
zend_register_long_constant:
procedure(name: PAnsiChar; name_len: uint; lval: Longint; flags: integer;
  module_number: integer; TSRMLS_DC: pointer); cdecl;

_emalloc:
function(size: size_t; __zend_filename: PAnsiChar; __zend_lineno: uint;
  __zend_orig_filename: PAnsiChar; __zend_orig_line_no: uint): pointer; cdecl;
zend_error:
procedure(ErrType: integer; ErrText: PAnsiChar);
cdecl varargs;
zend_eval_string:
function(str: PAnsiChar; val: pointer; strname: PAnsiChar; tsrm: pointer)
  : integer; cdecl;

array_init:
function(arg: pzval; __zend_filename: PAnsiChar; __zend_lineno: uint)
  : integer; cdecl;

ts_resource_ex:
function(id: integer; p: pointer): pointer; cdecl;

call_user_function:
function(function_table: PHashTable; object_pp: pzval; function_name: pzval;
  return_ptr: pzval; param_count: uint; params: pzval_array_ex;
  TSRMLS_DC: pointer): integer; cdecl;

_zval_dtor_func:
procedure(val: pzval; __zend_filename: PAnsiChar; __zend_lineno: uint); cdecl;

_convert_to_string:
procedure(op: pzval; __zend_filename: PAnsiChar; __zend_lineno: uint); cdecl;
zend_get_executed_lineno:
function(TSRMLS_DC: pointer): uint; cdecl;
{ zend_call_method:function(object_pp:Pzval; obj_ce:pointer; fn_proxy:pointer;
  function_name:PAnsiChar;function_name_len:size_t; retval:PZval; param_count:Integer;
  arg1:PZval; arg2:PZval):pzval; cdecl;
}

procedure ZVAL_TRUE(value: pzval);
procedure ZVAL_FALSE(value: pzval);
procedure ZvalString(z: pzval)overload;
procedure ZvalString(z: pzval; s: PAnsiChar; len: integer = 0)overload;
procedure ZvalString(z: pzval; s: PWideChar; len: integer = 0)overload;
procedure ZvalString(z: pzval; s: string; len: integer = 0)overload;
function HRESULTStr(h: HRESULT): Pchar;
procedure ZvalHRESULTStr(z: pzval; h: HRESULT);
function estrndup(s: PAnsiChar; len: Cardinal): PAnsiChar;
function ISPHPLib: boolean;
function LoadZEND(const DllFilename: AnsiString = DllPHP): boolean;
procedure UnloadZEND;
function zregister_long_constant(name: PAnsiChar; lval: Longint; flags: integer;
  module_number: integer; TSRMLS_DC: pointer): integer;
function ZvalInt(z: zval): integer;
function ZvalDouble(z: zval): double;
function ZvalBool(z: zval): boolean;
function ZvalPDouble(z: zval): PDouble;
function ZvalGetString(z: pzval): string; inline;

function ZvalGetStringA(z: pzval): AnsiString;
function ZvalGetStringW(z: pzval): WideString;
function ZvalGetStringRaw(z: pzval): RawByteString;

procedure ZvalVALStrNull(z: pzval); overload;
procedure ZvalVAL(z: pzval; s: AnsiString; len: integer = 0); overload;
procedure ZvalVAL(z: pzval; s: string; len: integer = 0); overload;

procedure ZvalVAL(z: pzval; v: boolean)overload;
procedure ZvalVAL(z: pzval; v: integer; const _type: integer = IS_LONG)overload;
procedure ZvalVAL(z: pzval)overload;
procedure ZvalVAL(z: pzval; v: double)overload;
procedure ZvalVAL(z: pzval; v: Extended); overload;

procedure Pre(str: string);
function ZvalArrayAdd(z: pzval; Args: array of const): integer; overload;
function ZvalArrayAdd(z: pzval; idx: integer; Args: array of const)
  : integer; overload;
function ZvalArrayAdd(z: pzval; key: AnsiString; Args: array of const)
  : integer; overload;

function ZValArrayKeyExists(v: pzval; key: AnsiString): boolean; overload;
function ZValArrayKeyExists(v: pzval; key: AnsiString; out pData: pzval)
  : boolean; overload;
function ZValArrayKeyExists(v: pzval; idx: integer): boolean; overload;
function ZValArrayKeyExists(v: pzval; idx: integer; out pData: pzval)
  : boolean; overload;
function ZValArrayKeyDel(v: pzval; key: AnsiString): boolean; overload;
function ZValArrayKeyDel(v: pzval; idx: integer): boolean; overload;

function ZValArrayKeyFind(v: pzval; key: AnsiString; out pData: ppzval)
  : boolean; overload;
function ZValArrayKeyFind(v: pzval; idx: integer; out pData: ppzval)
  : boolean; overload;

function GetArgPZval(Args: TVarRec; const _type: integer = IS_LONG;
  Make: boolean = false): pzval;

procedure ALLOC_ZVAL(out Result: pzval);
procedure INIT_PZVAL(p: pzval);
procedure MAKE_STD_ZVAL(out Result: pzval);

function emalloc(size: size_t): pointer;
function GetExecutorGlobals: Pzend_executor_globals;
procedure _zval_dtor(val: pzval; __zend_filename: PAnsiChar;
  __zend_lineno: uint);

implementation

function ISPHPLib: boolean;
begin
  Result := PHP5dll <> 0;
end;

function LoadZEND;
begin
  Result := false;
  if FileExists(string(DllFilename)) then
  begin
    PHP5dll := LoadLibraryA(PAnsiChar(DllFilename));
    if (PHP5dll <> 0) then
    begin
      zend_get_parameters_ex := GetProcAddress(PHP5dll,
        'zend_get_parameters_ex');
      _estrndup := GetProcAddress(PHP5dll, '_estrndup');

      zend_hash_func := GetProcAddress(PHP5dll, 'zend_hash_func');
      _zend_hash_quick_add_or_update := GetProcAddress(PHP5dll,
        '_zend_hash_quick_add_or_update');
      zend_hash_exists := GetProcAddress(PHP5dll, 'zend_hash_exists');
      zend_hash_index_exists := GetProcAddress(PHP5dll,
        'zend_hash_index_exists');
      zend_hash_del_key_or_index := GetProcAddress(PHP5dll,
        'zend_hash_del_key_or_index');
      zend_hash_quick_find := GetProcAddress(PHP5dll, 'zend_hash_quick_find');
      _emalloc := GetProcAddress(PHP5dll, '_emalloc');
      zend_hash_add_or_update := GetProcAddress(PHP5dll,
        '_zend_hash_add_or_update');
      zend_eval_string := GetProcAddress(PHP5dll, 'zend_eval_string');
      zend_register_long_constant := GetProcAddress(PHP5dll,
        'zend_register_long_constant');
      ts_resource_ex := GetProcAddress(PHP5dll, 'ts_resource_ex');
      call_user_function := GetProcAddress(PHP5dll, 'call_user_function');
      _zval_dtor_func := GetProcAddress(PHP5dll, '_zval_dtor_func');
      zend_error := GetProcAddress(PHP5dll, 'zend_error');
      _convert_to_string := GetProcAddress(PHP5dll, '_convert_to_string');

      array_init := GetProcAddress(PHP5dll, '_array_init');
      zend_wrong_param_count := GetProcAddress(PHP5dll,
        'zend_wrong_param_count');
      zend_get_executed_lineno := GetProcAddress(PHP5dll,
        'zend_get_executed_lineno');

      zend_hash_copy := GetProcAddress(PHP5dll, 'zend_hash_copy');
      // zend_call_method := GetProcAddress(PHP5dll, 'zend_call_method');

      Result := true;
    end;
  end;
end;

procedure _zval_dtor(val: pzval; __zend_filename: PAnsiChar;
  __zend_lineno: uint);
begin
  if val^._type <= IS_BOOL then
    Exit
  else
    _zval_dtor_func(val, __zend_filename, __zend_lineno);
end;

function GetGlobalResource(resource_name: AnsiString): pointer;
var
  global_id: pointer;
  global_value: integer;
  global_ptr: pointer;
  TSRMLS_DC: pointer;
begin
  Result := nil;
  try
    global_id := GetProcAddress(PHP5dll, PAnsiChar(resource_name));
    if Assigned(global_id) then
    begin
      TSRMLS_DC := ts_resource_ex(0, nil);
      global_value := integer(global_id^);
      asm
        mov ecx, global_value
        mov edx, dword ptr tsrmls_dc
        mov eax, dword ptr [edx]
        mov ecx, dword ptr [eax+ecx*4-4]
        mov global_ptr, ecx
      end;
      Result := global_ptr;
    end;
  except
    Result := nil;
  end;
end;

function GetExecutorGlobals: Pzend_executor_globals;
begin
  Result := GetGlobalResource('executor_globals_id');
end;

procedure UnloadZEND;
begin
  if ISPHPLib then
    FreeLibrary(PHP5dll);
end;

function estrndup;
begin
  if Assigned(s) then
    Result := _estrndup(s, len, nil, 0, nil, 0)
  else
    Result := nil;
end;

function HRESULTStr(h: HRESULT): Pchar;
begin
  FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_SYSTEM,
    nil, h, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), @Result, 0, nil);
end;

function ZvalInt;
var
  E: integer;
begin
  Case z._type of
    IS_LONG:
      Result := z.value.lval;
    IS_DOUBLE:
      Result := Trunc(z.value.dval);
    IS_BOOL:
      Result := Ord(not(z.value.lval = 0));
    IS_STRING:
      val(z.value.str.val, Result, E);
  else
    Result := 0;
  end;
end;

function zregister_long_constant(name: PAnsiChar; lval: Longint; flags: integer;
  module_number: integer; TSRMLS_DC: pointer): integer;
begin
  zend_register_long_constant(name, length(name) + 1, lval, flags,
    module_number, TSRMLS_DC);
end;

function valfloat(s: string; var err: integer): Extended;
var
  i: integer;
  x: Extended;
begin
  for i := 1 to length(s) do
    if s[i] = ',' then
      s[i] := '.';
  val(s, x, err);
  valfloat := x;
end;

function ZvalDouble;
var
  E: integer;
begin
  Case z._type of
    IS_LONG:
      Result := Trunc(z.value.lval);
    IS_DOUBLE:
      Result := z.value.dval;
    IS_BOOL:
      Result := Ord(not(z.value.lval = 0));
    IS_STRING:
      Result := valfloat(z.value.str.val, E);
  else
    Result := 0;
  end;
end;

function ZvalPDouble(z: zval): PDouble;
var
  x: double;
begin
  x := ZvalDouble(z);
  Result := PDouble(@x);
end;

function ZvalBool;
var
  E: integer;
begin
  Case z._type of
    IS_LONG:
      Result := not(z.value.lval = 0);
    IS_DOUBLE:
      Result := not(z.value.dval < 1);
    IS_BOOL:
      Result := not(z.value.lval = 0);
    IS_STRING:
      Result := not(z.value.str.len = 0);
  else
    Result := false;
  end;
end;

function ZvalGetStringRaw(z: pzval): RawByteString;
var
  r: PAnsiChar;
begin
  Result := '';
  case z._type of
    IS_BOOL:
      begin
        if z.value.lval = 0 then
          Result := RawByteString('False')
        else
          Result := RawByteString('True');
      end;
  else
    begin
      _convert_to_string(z, nil, 0);
      Result := RawByteString(z.value.str.val);
    end;
  end;
end;

function ZvalGetStringW(z: pzval): WideString;
begin
  Result := WideString(ZvalGetStringRaw(z));
end;

function ZvalGetStringA(z: pzval): AnsiString;
begin
  Result := ZvalGetStringRaw(z);
end;

function ZvalGetString(z: pzval): string; inline;
begin
  _convert_to_string(z, nil, 0);
  Result := string(z.value.str.val);
end;

procedure ZvalVAL(z: pzval; v: boolean);
Begin
  z._type := IS_BOOL;
  z.value.lval := integer(v);
End;

procedure ZvalVAL(z: pzval; v: integer; const _type: integer = IS_LONG);
Begin
  z._type := _type;
  z.value.lval := v;
End;

procedure ZvalVAL(z: pzval);
Begin
  z._type := IS_NULL;
End;

procedure ZvalVAL(z: pzval; v: double);
Begin
  z._type := IS_DOUBLE;
  z.value.dval := v;
End;

procedure ZvalVAL(z: pzval; v: Extended);
var
  D: double;
Begin
  D := v;
  z._type := IS_DOUBLE;
  z.value.dval := D;
End;

procedure ZvalVALStrNull(z: pzval);
begin
  z^.value.str.len := 0;
  z^.value.str.val := '';
  z^._type := IS_STRING;
end;

procedure ZvalVAL(z: pzval; s: string; len: integer = 0);
begin
  ZvalVAL(z, AnsiString(s), len);
end;

procedure ZvalVAL(z: pzval; s: AnsiString; len: integer = 0);
var
  lens: integer;
  AChar: PAnsiChar;
begin
  AChar := PAnsiChar(s);

  if not Assigned(AChar) then
    ZvalVALStrNull(z)
  else
  begin
    if len = 0 then
      lens := length(AChar)
    else
      lens := len;

    z^.value.str.len := lens;
    z^.value.str.val := _estrndup(AChar, lens, nil, 0, nil, 0);
    z^._type := IS_STRING;
  end;
end;

procedure ZVAL_TRUE;
begin
  value^._type := IS_BOOL;
  value^.value.lval := 1;
end;

procedure ZVAL_FALSE;
begin
  value^._type := IS_BOOL;
  value^.value.lval := 0;
end;

procedure ZvalString(z: pzval);
begin
  z^.value.str.len := 0;
  z^.value.str.val := '';
  z^._type := IS_STRING;
end;

procedure ZvalString(z: pzval; s: PAnsiChar; len: integer = 0);
var
  lens: integer;
begin
  if not Assigned(s) then
    ZvalString(z)
  else
  begin
    if len = 0 then
      lens := length(s)
    else
      lens := len;

    z^.value.str.len := lens;
    z^.value.str.val := estrndup(s, lens);
    z^._type := IS_STRING;
  end;
end;

procedure ZvalString(z: pzval; s: PWideChar; len: integer = 0);
begin
  if not Assigned(s) then
    ZvalString(z)
  else
    ZvalString(z, PAnsiChar(AnsiString(WideString(s))), len);
end;

procedure ZvalString(z: pzval; s: string; len: integer = 0);
var
  _s: PWideChar;
begin
  _s := PWideChar(s);

  if not Assigned(_s) then
    ZvalString(z)
  else
    ZvalString(z, _s, len);
end;

procedure ZvalHRESULTStr(z: pzval; h: HRESULT);
begin
  ZvalString(z, HRESULTStr(h));
end;

function emalloc(size: size_t): pointer;
begin
  Result := _emalloc(size, nil, 0, nil, 0);
end;

procedure ALLOC_ZVAL(out Result: pzval);
begin
  Result := emalloc(sizeof(zval));
end;

procedure INIT_PZVAL(p: pzval);
begin
  p^.refcount__gc := 1;
  p^.is_ref__gc := 0;
end;

procedure MAKE_STD_ZVAL(out Result: pzval);
begin
  ALLOC_ZVAL(Result);
  INIT_PZVAL(Result);
end;

function GetArgPZval;
begin
  if Args._Reserved1 = 0 then // nil
  begin
    if Make then
      MAKE_STD_ZVAL(Result);
    Result._type := IS_NULL;
  end
  else if Args.VType = vtPointer then
    Result := Args.VPointer
  else
  begin
    if Make then
      MAKE_STD_ZVAL(Result);
    case Args.VType of
      vtInteger:
        ZvalVAL(Result, Args.VInteger, _type);
      vtInt64:
        ZvalVAL(Result, NativeInt(Args.VInt64^), _type);
      vtBoolean:
        ZvalVAL(Result, Args.VBoolean);
      vtExtended:
        ZvalVAL(Result, Args.VExtended^);
      vtClass, vtObject:
        ZvalVAL(Result, Args._Reserved1);
      vtString:
        ZvalVAL(Result, AnsiString(Args.VString^));
      vtAnsiString:
        ZvalVAL(Result, PAnsiChar(Args.VAnsiString));
      vtUnicodeString:
        ZvalVAL(Result, UnicodeString(Args._Reserved1));
      vtWideChar:
        ZvalVAL(Result, AnsiString(Args.VWideChar));
      vtChar:
        ZvalVAL(Result, Args.VChar);
      vtPWideChar:
        ZvalVAL(Result, Args.VPWideChar);
      vtPChar:
        ZvalVAL(Result, Args.VPChar);
      vtWideString:
        ZvalVAL(Result, PWideChar(Args.VWideString));
    end;
  end;
end;

function zend_hash_add(ht: PHashTable; arKey: PAnsiChar; pData: pointer;
  nDataSize: uint; pDest: pointer): integer; cdecl;
begin
  Result := zend_hash_add_or_update(ht, arKey, length(arKey), pData, nDataSize,
    pDest, HASH_ADD);
end;

function AddElementZvalArray(z: pzval; Args: array of const; flag: integer;
  idx: uint = 0; len: uint = 0; const key: AnsiString = ''): integer;
var
  tmp: pzval;
  arKey: PAnsiChar;
begin
  Result := FAILURE;
  if z._type <> IS_ARRAY then
    Exit;

  if len <> 0 then
  begin
    inc(len);
    arKey := PAnsiChar(key);
    idx := zend_hash_func(arKey, len);
  end;

  tmp := GetArgPZval(Args[0], 1, true);
  Result := _zend_hash_quick_add_or_update(z.value.ht, arKey, len, idx, tmp,
    sizeof(pzval), nil, flag);
end;

// Add Next
function ZvalArrayAdd(z: pzval; Args: array of const): integer; overload;
begin
  Result := FAILURE;
  if z._type <> IS_ARRAY then
    Exit;
  Result := AddElementZvalArray(z, Args, HASH_NEXT_INSERT,
    z.value.ht.nNextFreeElement);
end;

// Add Index
function ZvalArrayAdd(z: pzval; idx: integer; Args: array of const)
  : integer; overload;
begin
  Result := AddElementZvalArray(z, Args, HASH_UPDATE, idx);
end;

// Add Assoc
function ZvalArrayAdd(z: pzval; key: AnsiString; Args: array of const)
  : integer; overload;
begin
  Result := AddElementZvalArray(z, Args, HASH_UPDATE, 0, length(key), key);
end;

function IsArrayRetVal(v: pzval): boolean;
begin
  Result := v._type = IS_ARRAY;
end;

function ZValArrayKeyExists(v: pzval; key: AnsiString): boolean; overload;
begin
  Result := false;
  if v._type <> IS_ARRAY then
    Exit;

  if v.value.ht.nNumOfElements = 0 then
    Exit;

  Result := zend_hash_exists(v.value.ht, PAnsiChar(key), length(key) + 1) = 1;
end;

function ZValArrayKeyExists(v: pzval; idx: integer): boolean; overload;
begin
  Result := false;
  if (v._type <> IS_ARRAY) then
    Exit;

  if v.value.ht.nNumOfElements = 0 then
    Exit;

  Result := zend_hash_index_exists(v.value.ht, idx) = 1;
end;

function ZValArrayKeyExists(v: pzval; key: AnsiString; out pData: pzval)
  : boolean; overload;
var
  tmp: ppzval;
begin
  Result := ZValArrayKeyExists(v, key);
  if Result then
  begin
    pData := nil;
    if ZValArrayKeyFind(v, key, tmp) then
      pData := tmp^;
  end;
end;

function ZValArrayKeyExists(v: pzval; idx: integer; out pData: pzval)
  : boolean; overload;
var
  tmp: ppzval;
begin
  Result := ZValArrayKeyExists(v, idx);
  if Result then
  begin
    pData := nil;
    if ZValArrayKeyFind(v, idx, tmp) then
      pData := tmp^;
  end;
end;

function ZValArrayKeyDel(v: pzval; key: AnsiString): boolean; overload;
begin
  Result := false;
  if ZValArrayKeyExists(v, key) then
    Result := zend_hash_del_key_or_index(v.value.ht, PAnsiChar(key),
      length(key) + 1, 0, HASH_DEL_KEY) = SUCCESS;
end;

function ZValArrayKeyDel(v: pzval; idx: integer): boolean; overload;
begin
  Result := false;
  if ZValArrayKeyExists(v, idx) then
    Result := zend_hash_del_key_or_index(v.value.ht, nil, 0, idx,
      HASH_DEL_INDEX) = SUCCESS;
end;

function ZValArrayKeyFind(v: pzval; key: AnsiString; out pData: ppzval)
  : boolean; overload;
var
  keyStr: PAnsiChar;
  KeyLength: uint;
begin
  keyStr := PAnsiChar(key);
  KeyLength := length(key) + 1;

  Result := zend_hash_quick_find(v.value.ht, keyStr, KeyLength,
    zend_hash_func(keyStr, KeyLength), pData) = SUCCESS;
end;

function ZValArrayKeyFind(v: pzval; idx: integer; out pData: ppzval)
  : boolean; overload;
begin
  Result := zend_hash_quick_find(v.value.ht, nil, 0, idx, pData) = SUCCESS;
end;

procedure Pre(str: string);
begin
  MessageBoxA(0, PAnsiChar(AnsiString(str)), '', 0)
end;

end.
