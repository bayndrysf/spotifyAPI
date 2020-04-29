% yusuf bayindir	
% 2017400042
% compiling: yes
% complete: yes

% artist(ArtistName, Genres, AlbumIds).
% album(AlbumId, AlbumName, ArtistNames, TrackIds).
% track(TrackId, TrackName, ArtistNames, AlbumName, [Explicit, Danceability, Energy,
%                                                    Key, Loudness, Mode, Speechiness,
%                                                    Acousticness, Instrumentalness, Liveness,
%                                                    Valence, Tempo, DurationMs, TimeSignature]).



features([explicit-0, danceability-1, energy-1,
          key-0, loudness-0, mode-1, speechiness-1,
       	  acousticness-1, instrumentalness-1,
          liveness-1, valence-1, tempo-0, duration_ms-0,
          time_signature-0]).


filter_features(Features, Filtered) :- features(X), filter_features_rec(Features, X, Filtered).
filter_features_rec([], [], []).
filter_features_rec([FeatHead|FeatTail], [Head|Tail], FilteredFeatures) :-
    filter_features_rec(FeatTail, Tail, FilteredTail),
    _-Use = Head,
    (
        (Use is 1, FilteredFeatures = [FeatHead|FilteredTail]);
        (Use is 0,
            FilteredFeatures = FilteredTail
        )
    ).



% remove_at(X,L,K,R) :- X is the K'th element of the list L; R is the
%  list that remains when the K'th element is removed from L.
% (element,list,integer,list) (?,?,+,?)

remove_at([_|Xs],1,Xs).
remove_at([Y|Xs],K,[Y|Ys]) :- K > 1,  K1 is K - 1, remove_at(Xs,K1,Ys).

% helperTrackIds(L,TrackIds) :- L is the list of album id's.
% Extracts trackk within the albums and return the list TrackIds.
% (list,list) (+,-)

helperTrackIds([],_):-!.
helperTrackIds( [Head|Tail] ,TempTrackIds ):-  album(Head,_,_,TempTrackList) , helperTrackIds(Tail,SubList),  append(TempTrackList, SubList,TempTrackIds).

%  helperTrackNames(+TrackIds,-TrackNames) :- Searches for 
%  names of the tracks within the list TrackIds an return 
%  the list of track names.
%  (list,list)


helperTrackNames([],_).
helperTrackNames( [X],TrackNames ) :- track(X,Name,_,_,_) ,TrackNames=[Name],!.
helperTrackNames( [Head|Tail], TrackNames) :-  track(Head,Name,_,_,_) , helperTrackNames(Tail,SubList), append([Name],SubList,TrackNames).

% getArtistTracks(+ArtistName, -TrackIds, -TrackNames) 5 points

getArtistTracks(ArtistName, TrackIds,TrackNames) :- artist(ArtistName,_, AlbumIds), helperTrackIds(AlbumIds,TempTrackIds), flatten(TempTrackIds,TempTrackIds2),
proper_length(TempTrackIds2,L),  remove_at(TempTrackIds2,L,TrackIds),  helperTrackNames(TrackIds,TrackNames).

/* ******************************* */



% albumFeatures(+AlbumId, -AlbumFeatures) 5 points

% getFeautureListOfLists(+TrackIds,-FeatureLists) Searches for
% feature lists of tracks within the list TrackIds and returns
% the list of feature lists as AlbumFeatures.
% (list,list)

getFeautureListOfLists([],_) :- !.
getFeautureListOfLists( TrackIds , FeatureList) :- findall( S , ( member(X,TrackIds), track(X,_,_,_,Unfiltered),filter_features( Unfiltered, S) ), FeatureList ).


% list_sum(+List1,+List2,-SumList) :- Given list of numbers List1 and List2,
% computes the sum respectively  returns SumList.
% ([a,b],[c,d],[ (a+c),(b+d) ] ) 
% (list,list,list)

list_sum([],[],[]).
list_sum([H1|T1],[H2|T2],[X|L3]):-list_sum(T1,T2,L3), X is H1+H2.

% divide(+List1,+Divisor,-List2) :- Given a list of numbers List1 and the
% divisor number Divisor, divides all elements of List1 and returns as List2.
% (list,integer,list)

divide([],_,[]).
divide( [Head|Tail], NumberOfTracks, [X|L]) :- divide(Tail,NumberOfTracks,L), X is Head / NumberOfTracks.

% compute(+FeatureListOfLists,+NumberOfTracks,-FeatureList) :- Given feature list of lists and
% number of tracks , computes the sum of each respective element returns as FeatureList. 
% After the execution of compute(list,number,list), divide(list,integer,list) should be called. 
% (list,number,list)

compute([],_, [0,0,0,0,0,0,0,0]).
compute( [Head|Tail]  ,NumberOfTracks, AlbumFeatureList) :- compute(Tail,NumberOfTracks,S), 
list_sum( Head,[0,0,0,0,0,0,0,0],TempSum),list_sum(TempSum,S, AlbumFeatureList).


albumFeatures(AlbumId,FeatureList) :- album(AlbumId,_,_,TempTrackIds), getFeautureListOfLists(TempTrackIds,TempFeatureList), 
proper_length(TempFeatureList,NumberOfTracks), compute(TempFeatureList,NumberOfTracks, TempFeatureList2),
divide(TempFeatureList2,NumberOfTracks,FeatureList).




% artistFeatures(+ArtistName, -ArtistFeatures) 5 points

artistFeatures(ArtistName,ArtistFeatures) :- getArtistTracks(ArtistName,TempTrackIds,_), getFeautureListOfLists(TempTrackIds,TempFeatureList),
proper_length(TempFeatureList,NumberOfTracks), compute(TempFeatureList,NumberOfTracks,TempFeatureList2),
divide(TempFeatureList2,NumberOfTracks,ArtistFeatures).



% trackDistance(+TrackId1, +TrackId2, -Score) 5 points

% score(+FeatureList1,+FeatureList2,-TempSumofElements) :- 
% Given two feature lists, returns the list of squares of the
% difference of each respective element within FeatureList1 and FeatureList2
% (list,list,list)

score([],[],[]).
score([H1|T1],[H2|T2],[X|L3]):- score(T1,T2,L3), X is (H1-H2)*(H1-H2).

% sum(+List,-Score) :- Given the return list TempSumofElements of score(list,list,list)
% returns sum of elements. 
% (list,number)


sum([],0).
sum([H|T],Score):- sum(T,TempSum), Score is H+TempSum. 


trackDistance(TrackId1,TrackId2,Score) :-  track(TrackId1,_,_,_,Unfiltered1) , track(TrackId2,_,_,_,Unfiltered2) ,
filter_features(Unfiltered1,Filtered1),filter_features(Unfiltered2,Filtered2),
score(Filtered1,Filtered2,TempSquaresList), sum(TempSquaresList,TempScore), Score is sqrt(TempScore).



% albumDistance(+AlbumId1, +AlbumId2, -Score) 5 points

albumDistance(AlbumId1,AlbumId2,Score) :- albumFeatures(AlbumId1,AlbumFeatures1),albumFeatures(AlbumId2,AlbumFeatures2),
score(AlbumFeatures1,AlbumFeatures2,TempSquaresList),sum(TempSquaresList,TempScore), Score is sqrt(TempScore).


% artistDistance(+ArtistName1, +ArtistName2, -Score) 5 points

artistDistance(Name1,Name2,Score) :- artistFeatures(Name1,Features1),artistFeatures(Name2,Features2),
score(Features1,Features2,TempSquaresList),sum(TempSquaresList,TempScore), Score is sqrt(TempScore).





% findMostSimilarTracks(+TrackId, -SimilarIds, -SimilarNames) 10 points

% slice(+List,+Start,+End,-ReturnList) :- Given a list and two numbers representing
% the starting and ending index of the list(inclusive), returns the sublist as ReturnList.
% (list,number,number,list)

slice([X|_],1,1,[X]).

slice([X|Xs],1,K,[X|Ys]) :- K > 1, 
   K1 is K - 1, slice(Xs,1,K1,Ys).
slice([_|Xs],I,K,Ys) :- I > 1, 
   I1 is I - 1, K1 is K - 1, slice(Xs,I1,K1,Ys).

% process(+TrackId,+ComparisonList,-PairedList) :- Given a track, simply paires 
% all elements of ComparisonList with their distance to the track having TrackId.
% (atom,list,list)


process(_,[],_):-!.
process(TrackId, [T|L1] , [D-V|L2] ) :- process(TrackId,L1,L2), trackDistance(TrackId,T,D),V=T.


findMostSimilarTracks(TrackId,SimilarIds,SimilarNames) :- findall( S , ( track(S,_,_,_,_) , S\==TrackId ), ComparisonList ), 
process(TrackId,ComparisonList,PairedComparisonList),length(PairedComparisonList,L),  remove_at(PairedComparisonList,L,PairedComparisonList1),
sort(1, @=<, PairedComparisonList1, SortedPairedComparisonList), pairs_values(SortedPairedComparisonList,Values), slice(Values,1,30,SimilarIds), findall( Name, ( member(B,SimilarIds), track(B,Name,_,_,_) ), SimilarNames),
trackDistance("20NEld0MhrR4Zpgrf9brq8","6CtnrcBHoK6Xf5rHUf0vbZ",Source),write(Source).


% findMostSimilarAlbums(+AlbumId, -SimilarIds, -SimilarNames) 10 points

% process1(+AlbumId,+ComparisonList,-PairedList) :- Given a track, simply paires 
% all elements of ComparisonList with their distance to the album having AlbumId.
% (atom,list,list)


process1(_,[],_):-!.
process1(AlbumId, [T|L1] , [D-V|L2] ) :- process1(AlbumId,L1,L2), albumDistance(AlbumId,T,D),V=T.

findMostSimilarAlbums(AlbumId,SimilarIds,SimilarNames) :- findall( S, ( album(S,_,_,_) , S\==AlbumId ), ComparisonList ), 
process1(AlbumId,ComparisonList,PairedComparisonList), length(PairedComparisonList,L),  remove_at(PairedComparisonList,L,PairedComparisonList1),
sort(1, @=<, PairedComparisonList1, SortedPairedComparisonList), pairs_values(SortedPairedComparisonList,Values), slice(Values,1,30,SimilarIds), findall( Name, ( member(B,SimilarIds), album(B,Name,_,_) ), SimilarNames).


% findMostSimilarArtists(+ArtistName, -SimilarArtists) 10 points

% process2(+ArtistName,+ComparisonList,-PairedList) :- Given an artist, simply paires 
% all elements of ComparisonList with their distance to the artist with ArtistName.
% (atom,list,list)

process2(_,[],_):-!.
process2(ArtistName, [T|L1] , [D-V|L2] ) :- process2(ArtistName,L1,L2), artistDistance(ArtistName,T,D),V=T.

findMostSimilarArtists(ArtistName,SimilarArtists) :- findall( S, ( artist(S,_,_) , S\==ArtistName ) , ComparisonList ),
process2(ArtistName,ComparisonList,PairedComparisonList),length(PairedComparisonList,L),  remove_at(PairedComparisonList,L,PairedComparisonList1),
sort(1, @=<, PairedComparisonList1, SortedPairedComparisonList), pairs_values(SortedPairedComparisonList,Values), slice(Values,1,30,SimilarArtists).



% filterExplicitTracks(+TrackList, -FilteredTracks) 5 points

% eliminate(+ListofFeatureLists,-EliminatedList) :- Giben list of feature lists, 
% eliminates the explicit songs and returns the id's of tracks as EliminatedList.
% (list,list)

eliminate( [],[]) :- !.
eliminate( [ [1|_]-_ | Tail ], L1):- eliminate(Tail,L1),!.
eliminate( [ [0|_]-T1 | Tail ], [Id|L]) :- eliminate(Tail,L), Id=T1. 

filterExplicitTracks(TrackList,FilteredTracks) :- findall( S-T1 , ( member(T1,TrackList), track(T1,_,_,_,S) ) , FeaturesListofLists ),
eliminate(FeaturesListofLists,FilteredTracks).



% getTrackGenre(+TrackId, -Genres) 5 points

getTrackGenre(TrackId,Genres):- track(TrackId,_,L,_,_), findall( S, ( member(X,L),artist(X,S,_) ), TempGenres ),
flatten(TempGenres,TempGenres2),list_to_set(TempGenres2,Genres).


% discoverPlaylist(+LikedGenres, +DislikedGenres, +Features, +FileName, -Playlist) 30 points


% likedGenres(+All,+LikedGenres) :- Given the list of genres of an artist 
% as All, and liked genres as LikedGenres , searches for the list.
% Returns false if no match is caught
% (list,list)
likedGenres(All,LikedGenres) :- findall( G , (  member(T0,All) , member(T1,LikedGenres), sub_string(T0,_,_,_,T1), G=T1 ), L2 ), 
length(L2,Length), Length \== 0.

% dislikedGenres(+All,+LikedGenres) :- Given the list of genres of an artist 
% as All, and liked genres as LikedGenres , searches for the list.
% Returns false if any match is caught
% (list,list)

dislikedGenres(All,DislikedGenres) :- findall( G, ( member(T0,All), 
member(T1,DislikedGenres) , sub_string(T0,_,_,_,T1), G=T1 ),L2), 
length(L2,Length), Length =:= 0.

% distanceCalc(+TrackIds,+UserFeatureList,-PairedList) :- Given the list of track id's 
% as TrackIds and the user feature list preference as UserFeatureList , paires all id's
% with their distance to the user's feature list and returns as PairedList.
% (list,list,list)

distanceCalc([],_,[]).
distanceCalc([H|Tail],Features,[D-H|L] ) :- distanceCalc(Tail,Features,L) , track(H,_,_,_,TempFeatures),filter_features(TempFeatures,Filtered),
score(Filtered,Features,TempSquaresList), sum(TempSquaresList,TempScore), D is sqrt(TempScore).

discoverPlaylist(LikedGenres,DislikedGenres,Features,FileName,Playlist) :- 
findall( Name, ( artist(Name,Genres,_), likedGenres(Genres,LikedGenres), dislikedGenres(Genres,DislikedGenres) ), L0 ), 
findall( S , ( member(TempArtistName,L0), getArtistTracks(TempArtistName,S,_) ), L1 ), flatten(L1,TempTrackIds), 
distanceCalc(TempTrackIds,Features,TempPairedComparisonList), list_to_set(TempPairedComparisonList, PairedSetComparison),
sort(1, @=<, PairedSetComparison, SortedPairedComparisonList),pairs_keys(SortedPairedComparisonList,TempKeys),
slice(TempKeys,1,30,Keys),pairs_values(SortedPairedComparisonList,Values),  slice(Values,1,30,Playlist), open(FileName,write,OS), 
writeln(OS,Playlist), findall( N-A1 , (  member(X,Playlist),track(X,N,A1,_,_) ) , TempPair ), pairs_keys(TempPair,Names), 
pairs_values(TempPair,ArtistNames),writeln(OS,Names),writeln(OS,ArtistNames),writeln(OS,Keys),close(OS).
