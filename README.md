# elm-forms

## Desenvolvimento técnico
O Dontform possui um sistema de persistência baseado em arquivos. Cada formulário é salvo em um arquivo no formato JSON, que pode ser consultado e atualizado de acordo com a necessidade.

Foi utilizado o Browser.application no front-end para facilitar a manipulação das urls e a integração com o backend

Foram utilizados, principalmente, dois Union types no front-end da aplicação. 
    
O tipo "FieldType" é responsável pela configuração de tipo do conteúdo de um determinado campo, seja ele numérico, textual, entre outros. 
    
Já o tipo "Value" provê a manipulação adequada dos diferentes conteúdos provenientes de um respectivo campo. Por exemplo, o processamento de um campo que possui como conteúdo um parâmetro booleano e um outro campo que possui como conteúdo uma String, pode ser executado pela mesma funcão graças a este Union Type.

## Qualidade do Produto
O programa permite a criação de fields e consegue validadar esses fields e receber respostas, entretanto a parte de visualizar essas respotas foi somente parcialmente implementada e por isso não é possível visualizar as respostas. 

## Integração front + back
Cada um dos formulários é criado em uma URL diferente, fornecida pelo usuário. Ao acessar esta URL, o Backend cria, dinamicamente, um arquivo no formato JSON com o nome coorrespondente ao endereço criado pelo usuário. Ao acessar este link novamente, o Backend carrega este arquivo, e envia ao Frontend o conteúdo deste. Em um mesmo serviço, podem ser criados vários formulários, sem interferências um nos outros.

O sistema não está integrado totalmente com o backend pois o endpoint de visualizar respotas não foi implementado



## Dependências

  Instale o Stack com curl -sSL https://get.haskellstack.org/ | sh

  Instale o Elm. Mais informações em https://guide.elm-lang.org/install.html

  Instale elm-live com `npm install --global elm-live`

## Como Usar

  Clone o front-end do projeto com o link: https://github.com/fga-funcional/dontmix.git

  Clone o back-end do prjeto o link: https://gitlab.com/LucasMartins/dontform.git

  Rode os comandos `elm-live src/App.elm --open --pushstate` e `devd -X /api/=http://localhost:3000 /=http://localhost:8000 -p 8080`

  Acesse `localhost:8000/anything` para o front end e `localhost:8000/api/anything` para o backend
