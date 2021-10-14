import Prim "mo:⛔";
import P "mo:base/Prelude";
import A "mo:base/Array";
import Hash "mo:base/Hash";
import Iter "mo:base/Iter";
import AssocList "mo:base/AssocList";

module {
    // key-val list type
    type KVs<K, V> = AssocList.AssocList<K, V>;

    public type HashMap<K, V> = {
        var table : [var KVs<K, V>];
        var _count : Nat;
    };

    public func size<K, V>(hm: HashMap<K, V>) : Nat = hm._count;

    public func defaults<K, V>() : HashMap<K, V> {
        return {
            var table : [var KVs<K, V>] = [var];
            var _count : Nat = 0;
        };
    };

    public func delete<K, V>(
        hm: HashMap<K, V>, 
        k: K, 
        keyHash: K -> Hash.Hash,
        keyEq : (K, K) -> Bool) : HashMap<K, V> = remove(hm, k, keyHash, keyEq).1;

    public func remove<K, V>(
        hm: HashMap<K, V>, 
        k: K, 
        keyHash: K -> Hash.Hash,
        keyEq : (K, K) -> Bool) : (?V, HashMap<K, V>) {

        let m = hm.table.size();
        if (m > 0) {
            let h = Prim.nat32ToNat(keyHash(k));
            let pos = h % m;
            let (kvs2, ov) = AssocList.replace<K, V>(hm.table[pos], k, keyEq, null);
            hm.table[pos] := kvs2;
            switch(ov){
            case null { };
            case _ { hm._count -= 1; }
            };
            (ov, hm)
        } else {
            (null, hm)
        };
    };

    public func get<K, V>(
        hm: HashMap<K, V>,
        k : K,
        keyHash: K -> Hash.Hash,
        keyEq : (K, K) -> Bool) : ?V {

        let h = Prim.nat32ToNat(keyHash(k));
        let m = hm.table.size();
        let v = if (m > 0) {
            AssocList.find<K, V>(hm.table[h % m], k, keyEq)
        } else {
            null
        };
    };

    public func put<K, V>(
        hm: HashMap<K, V>,
        k : K,
        v : V,
        keyHash: K -> Hash.Hash,
        keyEq : (K, K) -> Bool) : HashMap<K, V> = replace(hm, k, v, keyHash, keyEq).1;

    /// Insert the value `v` at key `k` and returns the previous value stored at
    /// `k` or `null` if it didn't exist.
    public func replace<K, V>(
        hm: HashMap<K, V>,
        k : K,
        v : V,
        keyHash: K -> Hash.Hash,
        keyEq : (K, K) -> Bool) : (?V, HashMap<K, V>) {

        if (hm._count >= hm.table.size()) {
            let size =
            if (hm._count == 0) {
                1
            } else {
                hm.table.size() * 2;
            };
            let table2 = A.init<KVs<K, V>>(size, null);
            for (i in hm.table.keys()) {
            var kvs = hm.table[i];
            label moveKeyVals : ()
            loop {
                switch kvs {
                case null { break moveKeyVals };
                case (?((k, v), kvsTail)) {
                    let h = Prim.nat32ToNat(keyHash(k));
                    let pos2 = h % table2.size();
                    table2[pos2] := ?((k,v), table2[pos2]);
                    kvs := kvsTail;
                };
                }
            };
            };
            hm.table := table2;
        };
        let h = Prim.nat32ToNat(keyHash(k));
        let pos = h % hm.table.size();
        let (kvs2, ov) = AssocList.replace<K, V>(hm.table[pos], k, keyEq, ?v);
        hm.table[pos] := kvs2;
        switch(ov){
            case null { hm._count += 1 };
            case _ {}
        };
        (ov, hm)
    };

    /// Returns an iterator over the key value pairs in this
    /// `HashMap`. Does _not_ modify the `HashMap`.
    public func entries<K,V>(        
        hm: HashMap<K, V>, 
    ) : Iter.Iter<(K, V)> {
        var table = hm.table;
        if (table.size() == 0) {
            object { public func next() : ?(K, V) { null } }
        }
        else {
            object {
                var kvs = table[0];
                var nextTablePos = 1;
                public func next () : ?(K, V) {
                    switch kvs {
                        case (?(kv, kvs2)) {
                            kvs := kvs2;
                            ?kv
                        };
                        case null {
                            if (nextTablePos < table.size()) {
                            kvs := table[nextTablePos];
                            nextTablePos += 1;
                            next()
                            } else {
                                null
                            }
                        }
                    }
                }
            }
        }
    };



};
