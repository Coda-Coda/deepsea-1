(module
  (type (;0;) (func (param ) (result i32)))
  (type (;1;) (func (param ) (result i32)))
  (import  "ethereum" "getAddress"  (func $f0 (param i32) (result )))
  (import  "ethereum" "getExternalBalance"  (func $f1 (param i32 i32) (result )))
  (import  "ethereum" "getBlockHash"  (func $f2 (param i64 i32) (result i32)))
  (import  "ethereum" "callDataCopy"  (func $f4 (param i32 i32 i32) (result )))
  (import  "ethereum" "getCallDataSize"  (func $f5 (param ) (result i32)))
  (import  "ethereum" "callDelegate"  (func $f7 (param i64 i32 i32 i32) (result i32)))
  (import  "ethereum" "storageStore"  (func $f8 (param i32 i32) (result )))
  (import  "ethereum" "storageLoad"  (func $f9 (param i32 i32) (result )))
  (import  "ethereum" "getCaller"  (func $f10 (param i32) (result )))
  (import  "ethereum" "getCallValue"  (func $f11 (param i32) (result )))
  (import  "ethereum" "getBlockCoinbase"  (func $f14 (param i32) (result i32)))
  (import  "ethereum" "getBlockDifficulty"  (func $f16 (param i32) (result )))
  (import  "ethereum" "getGasLeft"  (func $f19 (param ) (result i64)))
  (import  "ethereum" "getBlockGasLimit"  (func $f20 (param ) (result i64)))
  (import  "ethereum" "getTxGasPrice"  (func $f21 (param i32) (result )))
  (import  "ethereum" "log"  (func $f22 (param i32 i32 i32 i32 i32 i32 i32) (result )))
  (import  "ethereum" "getBlockNumber"  (func $f23 (param ) (result i64)))
  (import  "ethereum" "getTxOrigin"  (func $f24 (param i32) (result )))
  (import  "ethereum" "useGas"  (func $f25 (param i64) (result )))
  (import  "ethereum" "getBlockTimestamp"  (func $f27 (param ) (result i64)))
  (import  "ethereum" "revert"  (func $f28 (param i32 i32) (result )))
  (import  "ethereum" "getReturnDataSize"  (func $f29 (param ) (result i32)))
  (import  "ethereum" "returnDataCopy"  (func $f30 (param i32 i32 i32) (result )))
  (import  "ethereum" "call"  (func $f3 (param i64 i32 i32 i32 i32) (result i32)))
  (import  "ethereum" "finish"  (func $finish (param i32 i32) (result )))
  (memory (;0;) 20)
  (data (i32.const 0) "0")
  (data (i32.const 256) "0")
  (data (i32.const 512) "0")
  (data (i32.const 768) "0")
  (data (i32.const 1024) "0")
  (data (i32.const 1280) "0")
  (data (i32.const 1536) "0")
  (data (i32.const 1792) "0")
  (data (i32.const 2048) "0")
  (global (mut i32) (i32.const 0))  (global (mut i32) (i32.const 0))  (global (mut i32) (i32.const 0))
  (export "memory" (memory 0))
  (export "main" (func $main))
  (func $f128 (param $0 i32) (param $1 i32) (result i32)
    (local $2 i32)
    (local $3 i32)
    (local.set $3
    (select
      (local.get $0)
      (i32.const 1)
      (i32.and
      (local.get $1)
      (i32.const 1)
      )
    )
    )
    (block $label$0
    (br_if $label$0
      (i32.eqz
      (local.tee $1
        (i32.shr_s
        (local.get $1)
        (i32.const 1)
        )
      )
      )
    )
    (loop $label$1
      (local.set $3
      (i32.mul
        (select
        (local.tee $0
          (i32.mul
          (local.get $0)
          (local.get $0)
          )
        )
        (i32.const 1)
        (i32.and
          (local.get $1)
          (i32.const 1)
        )
        )
        (local.get $3)
      )
      )
      (local.set $1
      (local.tee $2
        (i32.shr_s
        (local.get $1)
        (i32.const 1)
        )
      )
      )
      (br_if $label$1
      (local.get $2)
      )
    )
    )
    (local.get $3))
  (func $f129 (param $p0 i32) (param $p1 i32) (result i32) 
      local.get 0
    )
  (func $f130 (param $p0 i32) (param $p1 i32) (result i32) 
      local.get 0
    )
  (func $f132 (param $p0 i32) (result i32)
    local.get $p0
    i32.const -1
    i32.xor)
  (func $set_returndata (param $value i32) (result )
    i32.const 256 ;; offset to store
    local.get $value ;; valut to store
    i32.store
  )
  (func $fallback (param ) (result) 
    i32.const 256
    i32.const 4
    call $f28
    )
  (func $chendian32 (param $p0 i32) (result i32)
    local.get $p0
    i32.const 24
    i32.shl
    local.get $p0
    i32.const 8
    i32.shl
    i32.const 16711680
    i32.and
    i32.or
    local.get $p0
    i32.const 8
    i32.shr_u
    i32.const 65280
    i32.and
    local.get $p0
    i32.const 24
    i32.shr_u
    i32.or
    i32.or)
  (func $main (param ) (result )
    call $f5
    global.set 0
    global.get 0
    i32.const 3
    i32.le_u
    if 
      i32.const 21
      call $set_returndata
      call $fallback
    else 
      nop
    end
    global.get 0
    i32.const 4
    i32.sub
    global.set 0
    i32.const 4
    global.set 1
    i32.const 768
    i32.const 0
    i32.const 4
    call $f4
    i32.const 768
    i32.load
    call $chendian32
    global.set 2
    global.get 2
    i32.const 0x26121ff0
    i32.eq
    if 
      global.get 0
      i32.const 0
      i32.eq
      i32.eqz
      if 
        i32.const 220 
        call $set_returndata
        call $fallback
      else 
        nop
      end
      i32.const 32 ;; 4 + 7 * 4 = 32, setting offset exclude name
      global.set 1

      (call $f257)
      global.set 1
      i32.const 256
      global.get 1
      i32.store
      i32.const 256
      i32.const 4
      (call $finish)
    else
      i32.const 22
      call $set_returndata
      (call $fallback)
      end

  )
  (func $constructor (type 0)
    (local $l0 i32)
    i32.const 0x0920
    i32.const 0x0940
    i32.store
    i32.const 0x0920
    i32.const 0x0920
    i32.load
    i32.const 0x00
    i32.add
    i32.store
    nop
    nop
    i32.const 0x0920
    i32.const 0x0920
    i32.load
    i32.const 0x00
    i32.sub
    i32.store
    i32.const 0
    return
  )
  (func $f257 (type 1)
    (local $l0 i32)
    i32.const 0x0920
    i32.const 0x0940
    i32.store
    i32.const 0x0920
    i32.const 0x0920
    i32.load
    i32.const 0x00
    i32.add
    i32.store
    i32.const 0x2a
    global.set 0
    i32.const 256
    global.get 0
    i32.store
    i32.const 0xbda66450
    i32.const 0x07b206cb
    i32.const 0x3d1d4b9c
    i32.const 0xff02f83b
    i32.const 0xec10ba1d
    i32.const 0xf7dcfa5e
    i32.const 0x43f14e97
    i32.const 0x47e26897
    global.set 0
    i32.const 1308
    global.get 0
    i32.store
    global.set 0
    i32.const 1304
    global.get 0
    i32.store
    global.set 0
    i32.const 1300
    global.get 0
    i32.store
    global.set 0
    i32.const 1296
    global.get 0
    i32.store
    global.set 0
    i32.const 1292
    global.get 0
    i32.store
    global.set 0
    i32.const 1288
    global.get 0
    i32.store
    global.set 0
    i32.const 1284
    global.get 0
    i32.store
    global.set 0
    i32.const 1280
    global.get 0
    i32.store
    i32.const 256
    i32.const 4
    i32.const 1
    i32.const 1280
    i32.const 1536
    i32.const 1792
    i32.const 2048
    call $f22
    i32.const 0x01
    i32.const 0x00
    global.set 0
    i32.const 260
    global.get 0
    i32.store
    global.set 0
    i32.const 256
    global.get 0
    i32.store
    i32.const 0x0afaf95c
    i32.const 0xbdfe749f
    i32.const 0xf2389a6d
    i32.const 0x3f7be0f0
    i32.const 0x2178a3d8
    i32.const 0x7c4195e6
    i32.const 0x0af7fae2
    i32.const 0xac1f52df
    global.set 0
    i32.const 1308
    global.get 0
    i32.store
    global.set 0
    i32.const 1304
    global.get 0
    i32.store
    global.set 0
    i32.const 1300
    global.get 0
    i32.store
    global.set 0
    i32.const 1296
    global.get 0
    i32.store
    global.set 0
    i32.const 1292
    global.get 0
    i32.store
    global.set 0
    i32.const 1288
    global.get 0
    i32.store
    global.set 0
    i32.const 1284
    global.get 0
    i32.store
    global.set 0
    i32.const 1280
    global.get 0
    i32.store
    i32.const 256
    i32.const 8
    i32.const 1
    i32.const 1280
    i32.const 1536
    i32.const 1792
    i32.const 2048
    call $f22
    i32.const 0xf0b631cf
    i32.const 0x252a8155
    i32.const 0x1aa966f6
    i32.const 0x6c88fed0
    i32.const 0x64439e20
    i32.const 0x83d99379
    i32.const 0xfed5e1d9
    i32.const 0xb8ce3b56
    i32.const 0x0d
    i32.const 0x00
    i32.const 0x00
    i32.const 0x00
    i32.const 0x00
    i32.const 0x00
    i32.const 0x00
    i32.const 0x00
    global.set 0
    i32.const 1308
    global.get 0
    i32.store
    global.set 0
    i32.const 1304
    global.get 0
    i32.store
    global.set 0
    i32.const 1300
    global.get 0
    i32.store
    global.set 0
    i32.const 1296
    global.get 0
    i32.store
    global.set 0
    i32.const 1292
    global.get 0
    i32.store
    global.set 0
    i32.const 1288
    global.get 0
    i32.store
    global.set 0
    i32.const 1284
    global.get 0
    i32.store
    global.set 0
    i32.const 1280
    global.get 0
    i32.store
    global.set 0
    i32.const 1564
    global.get 0
    i32.store
    global.set 0
    i32.const 1560
    global.get 0
    i32.store
    global.set 0
    i32.const 1556
    global.get 0
    i32.store
    global.set 0
    i32.const 1552
    global.get 0
    i32.store
    global.set 0
    i32.const 1548
    global.get 0
    i32.store
    global.set 0
    i32.const 1544
    global.get 0
    i32.store
    global.set 0
    i32.const 1540
    global.get 0
    i32.store
    global.set 0
    i32.const 1536
    global.get 0
    i32.store
    i32.const 256
    i32.const 0
    i32.const 2
    i32.const 1280
    i32.const 1536
    i32.const 1792
    i32.const 2048
    call $f22
    i32.const 0x02
    global.set 0
    i32.const 256
    global.get 0
    i32.store
    i32.const 0x50188836
    i32.const 0x2e16fcbe
    i32.const 0xcc5fd343
    i32.const 0x268a654f
    i32.const 0xd0a91278
    i32.const 0xdc075122
    i32.const 0x82644260
    i32.const 0xa7f9cd68
    i32.const 0x00
    i32.const 0x00
    i32.const 0x00
    i32.const 0x00
    i32.const 0x00
    i32.const 0x00
    i32.const 0x00
    i32.const 0x00
    global.set 0
    i32.const 1308
    global.get 0
    i32.store
    global.set 0
    i32.const 1304
    global.get 0
    i32.store
    global.set 0
    i32.const 1300
    global.get 0
    i32.store
    global.set 0
    i32.const 1296
    global.get 0
    i32.store
    global.set 0
    i32.const 1292
    global.get 0
    i32.store
    global.set 0
    i32.const 1288
    global.get 0
    i32.store
    global.set 0
    i32.const 1284
    global.get 0
    i32.store
    global.set 0
    i32.const 1280
    global.get 0
    i32.store
    global.set 0
    i32.const 1564
    global.get 0
    i32.store
    global.set 0
    i32.const 1560
    global.get 0
    i32.store
    global.set 0
    i32.const 1556
    global.get 0
    i32.store
    global.set 0
    i32.const 1552
    global.get 0
    i32.store
    global.set 0
    i32.const 1548
    global.get 0
    i32.store
    global.set 0
    i32.const 1544
    global.get 0
    i32.store
    global.set 0
    i32.const 1540
    global.get 0
    i32.store
    global.set 0
    i32.const 1536
    global.get 0
    i32.store
    i32.const 256
    i32.const 4
    i32.const 2
    i32.const 1280
    i32.const 1536
    i32.const 1792
    i32.const 2048
    call $f22
    i32.const 0x2a
    global.set 0
    i32.const 256
    global.get 0
    i32.store
    i32.const 0xe9c104f6
    i32.const 0x32f8554d
    i32.const 0x7e9a90f0
    i32.const 0x3982e0de
    i32.const 0xf7601d3b
    i32.const 0xd57e1e7b
    i32.const 0x085accf2
    i32.const 0xaf6151f5
    i32.const 0x01
    i32.const 0x00
    i32.const 0x00
    i32.const 0x00
    i32.const 0x00
    i32.const 0x00
    i32.const 0x00
    i32.const 0x00
    i32.const 0x02
    i32.const 0x00
    i32.const 0x00
    i32.const 0x00
    i32.const 0x00
    i32.const 0x00
    i32.const 0x00
    i32.const 0x00
    global.set 0
    i32.const 1308
    global.get 0
    i32.store
    global.set 0
    i32.const 1304
    global.get 0
    i32.store
    global.set 0
    i32.const 1300
    global.get 0
    i32.store
    global.set 0
    i32.const 1296
    global.get 0
    i32.store
    global.set 0
    i32.const 1292
    global.get 0
    i32.store
    global.set 0
    i32.const 1288
    global.get 0
    i32.store
    global.set 0
    i32.const 1284
    global.get 0
    i32.store
    global.set 0
    i32.const 1280
    global.get 0
    i32.store
    global.set 0
    i32.const 1564
    global.get 0
    i32.store
    global.set 0
    i32.const 1560
    global.get 0
    i32.store
    global.set 0
    i32.const 1556
    global.get 0
    i32.store
    global.set 0
    i32.const 1552
    global.get 0
    i32.store
    global.set 0
    i32.const 1548
    global.get 0
    i32.store
    global.set 0
    i32.const 1544
    global.get 0
    i32.store
    global.set 0
    i32.const 1540
    global.get 0
    i32.store
    global.set 0
    i32.const 1536
    global.get 0
    i32.store
    global.set 0
    i32.const 1820
    global.get 0
    i32.store
    global.set 0
    i32.const 1816
    global.get 0
    i32.store
    global.set 0
    i32.const 1812
    global.get 0
    i32.store
    global.set 0
    i32.const 1808
    global.get 0
    i32.store
    global.set 0
    i32.const 1804
    global.get 0
    i32.store
    global.set 0
    i32.const 1800
    global.get 0
    i32.store
    global.set 0
    i32.const 1796
    global.get 0
    i32.store
    global.set 0
    i32.const 1792
    global.get 0
    i32.store
    i32.const 256
    i32.const 4
    i32.const 3
    i32.const 1280
    i32.const 1536
    i32.const 1792
    i32.const 2048
    call $f22
    i32.const 0x2a
    global.set 0
    i32.const 256
    global.get 0
    i32.store
    i32.const 0x044eb09a
    i32.const 0x189f37d7
    i32.const 0x257438a9
    i32.const 0x7e119d43
    i32.const 0xea5b4ebd
    i32.const 0xbfe7067d
    i32.const 0xdd1adfa5
    i32.const 0x08aaf4f7
    i32.const 0x01
    i32.const 0x00
    i32.const 0x00
    i32.const 0x00
    i32.const 0x00
    i32.const 0x00
    i32.const 0x00
    i32.const 0x00
    i32.const 0x02
    i32.const 0x00
    i32.const 0x00
    i32.const 0x00
    i32.const 0x00
    i32.const 0x00
    i32.const 0x00
    i32.const 0x00
    global.set 0
    i32.const 1308
    global.get 0
    i32.store
    global.set 0
    i32.const 1304
    global.get 0
    i32.store
    global.set 0
    i32.const 1300
    global.get 0
    i32.store
    global.set 0
    i32.const 1296
    global.get 0
    i32.store
    global.set 0
    i32.const 1292
    global.get 0
    i32.store
    global.set 0
    i32.const 1288
    global.get 0
    i32.store
    global.set 0
    i32.const 1284
    global.get 0
    i32.store
    global.set 0
    i32.const 1280
    global.get 0
    i32.store
    global.set 0
    i32.const 1564
    global.get 0
    i32.store
    global.set 0
    i32.const 1560
    global.get 0
    i32.store
    global.set 0
    i32.const 1556
    global.get 0
    i32.store
    global.set 0
    i32.const 1552
    global.get 0
    i32.store
    global.set 0
    i32.const 1548
    global.get 0
    i32.store
    global.set 0
    i32.const 1544
    global.get 0
    i32.store
    global.set 0
    i32.const 1540
    global.get 0
    i32.store
    global.set 0
    i32.const 1536
    global.get 0
    i32.store
    global.set 0
    i32.const 1820
    global.get 0
    i32.store
    global.set 0
    i32.const 1816
    global.get 0
    i32.store
    global.set 0
    i32.const 1812
    global.get 0
    i32.store
    global.set 0
    i32.const 1808
    global.get 0
    i32.store
    global.set 0
    i32.const 1804
    global.get 0
    i32.store
    global.set 0
    i32.const 1800
    global.get 0
    i32.store
    global.set 0
    i32.const 1796
    global.get 0
    i32.store
    global.set 0
    i32.const 1792
    global.get 0
    i32.store
    i32.const 256
    i32.const 4
    i32.const 3
    i32.const 1280
    i32.const 1536
    i32.const 1792
    i32.const 2048
    call $f22
    i32.const 0x01
    local.set 0
    i32.const 0x0920
    i32.const 0x0920
    i32.load
    i32.const 0x00
    i32.sub
    i32.store
    local.get 0
    return
  )
)
