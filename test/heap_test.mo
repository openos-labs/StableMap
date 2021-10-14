import h "../test/StableHeap";
import N "mo:base/Nat";

actor heap_test{

    var heap = h.default<Nat>();

    public query func test() : async (){
        //put
        heap := h.put<Nat>(
                heap,
                1,
                N.compare
            );
        assert(
            h.peekMin(
                heap,
                N.compare
            ) == ?1);

        heap := h.put(
            heap,
            2,
            N.compare
        );
        assert(
            h.peekMin(
                heap,
                N.compare
            ) == ?1);        

        heap := switch(h.deleteMin(
            heap,
            N.compare
        )){
            case null { null };
            case (?h) { h };
        };
        assert(
            h.peekMin(
                heap,
                N.compare
            ) == ?2);

    //todo

    };



};