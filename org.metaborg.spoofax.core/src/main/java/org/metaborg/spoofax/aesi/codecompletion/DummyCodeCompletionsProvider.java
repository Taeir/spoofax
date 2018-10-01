package org.metaborg.spoofax.aesi.codecompletion;

import org.metaborg.aesi.ScopeNames;
import org.metaborg.aesi.SourceToken;
import org.metaborg.spoofax.core.unit.ISpoofaxAnalyzeUnit;
import org.metaborg.spoofax.core.unit.ISpoofaxParseUnit;

import javax.annotation.Nullable;
import java.util.Arrays;
import java.util.List;

public class DummyCodeCompletionsProvider implements ICodeCompletionsProvider {
    @Override
    public List<SpoofaxCodeCompletionProposal> getCompletions(
            @Nullable ISpoofaxParseUnit parseResult,
            @Nullable ISpoofaxAnalyzeUnit analysisResult,
            int caretOffset,
            SourceToken prefix) {
        /*
        let
        var N := 8
        type intArray = array of int
        var row := intArray [ N ] of 0
        var col := intArray [ N ] of 0
        var diag1 := intArray [ N+N-1 ] of 0
        var diag2 := intArray [ N+N-1 ] of 0

        function printboard() =
                (for i := 0 to N-1
        do (for j := 0 to N-1
        do print(if col[i]=j then " O" else " .");
        print("\n"));
        print("\n"))


        function try(c:int) =
        if c=N then printboard()
		else for r := 0 to N-1
        do if row[r]=0 &
                diag1[r+c]=0 & diag2[r+7-c]=0
        then (row[r] := 1; diag1[r+c] := 1;
        diag2[r+7-c] := 1; col[c] := r;
        try(c+1);
        row[r] := 0; diag1[r+c] := 0;
        diag2[r+7-c] := 0)
        in try(0) end
         */
        return Arrays.asList(
                new SpoofaxCodeCompletionProposal("c", null, null, null, new ScopeNames("variable"), null),
                new SpoofaxCodeCompletionProposal("col", null, null, null, new ScopeNames("field"), null),
                new SpoofaxCodeCompletionProposal("diag1", null, null, null, new ScopeNames("field"), null),
                new SpoofaxCodeCompletionProposal("diag2", null, null, null, new ScopeNames("field"), null),
                new SpoofaxCodeCompletionProposal("N", null, null, null, new ScopeNames("variable"), null),
                new SpoofaxCodeCompletionProposal("printboard", null, "Description", "right", new ScopeNames("method"), null),
                new SpoofaxCodeCompletionProposal("row", null, null, null, new ScopeNames("field"), null),
                new SpoofaxCodeCompletionProposal("try", null, null, null, new ScopeNames("method"), null)


        );
    }
}
